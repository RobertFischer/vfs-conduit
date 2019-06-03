{-|
Description: VFS persisted in-memory. This is intended to demonstrate how a VFS can be defined, but is also entirely usable in code
if you find its functionality useful.

This VFS implementation stores its values in a lazy in-memory map, which is itself persisted into an 'MVar' and captured in the monadic state.
This requires the monad to be in the class 'MonadUnliftIO', but that allows the state to be shared throughout the application, maintaining
value consistency across threads and even across invocations.

Individual read and write operations are atomic.

The 'FilePath' values used in this VFS are split using 'splitPath' and joined using '</>', but are otherwise used directly: there is no concept of
paths being "relative" or "absolute" for this VFS.  It is also possible for a file and a directory to have the same name, since directories names
are appended with @/@, as per 'splitPath'.  (This implementation detail is up for debate and may be changed in a future major release: please file
an issue if you want to have a discussion around it.)

__Note__: If you want to use this in production, I would encourage you to flesh out the implementations of the 'VFSC' typeclasses.
The default implementations do work and the performance is probably fine for all intents and purposes, but the default
implementations require unnecessary 'MVar' accesses, which could concievably become a performance bottleneck. If you do flesh out the
implementations, please submit a PR back to upstream and we will be most grateful. Thanks!

-}
module Data.Conduit.VFS.InMemory
	( InMemoryVFS
	, InMemoryVFSRoot
	, runInMemoryVFS
	, runInMemoryVFS'
	, mkInMemoryVFSRoot
	, module Data.Conduit.VFS.Import
	) where

import ClassyPrelude hiding (ByteString, handle)
import Data.Conduit.VFS.Import
import UnliftIO.Temporary (withSystemTempFile)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)
import System.FilePath (splitPath)
import System.IO (IOMode(ReadMode), hSetBinaryMode)
import Control.Monad.Trans.Resource (runResourceT)

-- | The possible kinds of nodes
data IMNode
	= IMNodeDir IMDirectory -- ^ VDirectory
	| IMNodeFile IMFile     -- ^ VFile

-- | How is the data stored?
data IMFile
	= Resident ByteString  -- ^ Data is stored resident in memory
	| EmptyFile            -- ^ Data is empty

-- | Definition of a directory
newtype IMDirectory = IMDirectory
	{ imdNodes :: HashMap String IMNode   -- ^ The nodes contained within the directory
	}

instance Semigroup IMDirectory where
	-- | Right-biased, but with directories recursively merged.
	(<>) imdLeft imdRight =
			IMDirectory { imdNodes = HashMap.unionWith mergeImpl nodeLeft nodeRight }
		where
			nodeLeft = imdNodes imdLeft
			nodeRight = imdNodes imdRight
			mergeImpl (IMNodeDir nodeDirLeft) (IMNodeDir nodeDirRight) = IMNodeDir $ nodeDirLeft <> nodeDirRight -- Recursively merge two dirs
			mergeImpl _ right = right -- Default to the right if there's any other kind of conflict.
	{-# INLINE (<>) #-}

instance Monoid IMDirectory where
	mempty = IMDirectory { imdNodes = mempty }

type instance Element IMDirectory = (String, IMNode)

instance MonoFunctor IMDirectory where
	omap f oldImd = IMDirectory{ imdNodes = HashMap.fromList (f <$> HashMap.toList (imdNodes oldImd)) }
	{-# INLINE omap #-}

{- TODO Implement the MonoFoldable methods.
instance MonoFoldable IMDirectory where
	otoList IMDirectory{imdNodes} = toList imdNodes
	{-# INLINE otoList #-}
-}

instance MonoPointed IMDirectory where
	opoint (k,v) = IMDirectory { imdNodes = HashMap.singleton k v }
	{-# INLINE opoint #-}

-- | The root of the VFS.
newtype InMemoryVFSRoot = InMemoryVFSRoot { imvfsStore :: MVar IMDirectory }

-- | The basic implementation of the VFS.
newtype InMemoryVFS m a = InMemoryVFS
	{ unIMVFS :: ReaderT InMemoryVFSRoot m a }
	deriving (Functor, Monad, MonadTrans, MonadIO)

instance (Functor f) => Applicative (InMemoryVFS f)
instance (Monad m) => MonadReader InMemoryVFSRoot (InMemoryVFS m)
instance (Functor m, MonadIO m) => MonadUnliftIO (ConduitT i o (InMemoryVFS m))

-- | Creates an 'InMemoryVFSRoot' that can be shared among many 'InMemoryVFS' invocations.
mkInMemoryVFSRoot :: (MonadIO m) => m InMemoryVFSRoot
mkInMemoryVFSRoot = do
	mvar <- newMVar mempty
	return $ InMemoryVFSRoot { imvfsStore = mvar }
{-# INLINE mkInMemoryVFSRoot #-}

-- | Given an 'InMemoryVFS', run it in the local monad.
runInMemoryVFS :: (MonadUnliftIO m) => InMemoryVFS m a -> m a
runInMemoryVFS imvfs = mkInMemoryVFSRoot >>= flip runInMemoryVFS' imvfs
{-# INLINE runInMemoryVFS #-}

-- | Runs an 'InMemoryVFS' using a provided 'InMemoryVFSRoot'.
runInMemoryVFS' :: InMemoryVFSRoot -> InMemoryVFS m a -> m a
runInMemoryVFS' root imvfs =
	let monad = unIMVFS imvfs in
	runReaderT monad root
{-# INLINE runInMemoryVFS' #-}

-- | Takes a function that consumes the root dir and produces a monadic action, and then returns that monadic
--   action as an 'InMemoryVFS'.
withIMVFSRootDir :: (MonadUnliftIO m) => ( IMDirectory -> m a ) -> InMemoryVFS m a
withIMVFSRootDir f = do
		mvar <- imvfsStore <$> ask
		rootDir <- readMVar mvar
		lift $ f rootDir
{-# INLINE withIMVFSRootDir #-}

-- | Takes a function that consumes the root dir, produces a new root dir as a monadic action, and then returns
--   that monadic action as an 'InMemoryVFS'.
modifyIMVFSRootDir :: (MonadUnliftIO m) => ( IMDirectory -> m IMDirectory ) -> InMemoryVFS m ()
modifyIMVFSRootDir f = do
	mvar <- imvfsStore <$> ask
	lift $ modifyMVar_ mvar f
{-# INLINE modifyIMVFSRootDir #-}

instance (MonadUnliftIO m) => ReadVFSC (InMemoryVFS m) where

	vfsTypeC = awaitForever $ \filepath -> do
			result <- lift $ withIMVFSRootDir $ return . loop (splitPath filepath)
			yield (filepath, result)
		where
			loop :: [FilePath] -> IMDirectory -> Maybe VFileType
			loop [] (IMDirectory _) = Just VDirectory
			loop ("/":rest) imd = loop rest imd
			loop (nextDirPath:rest) IMDirectory{imdNodes} =
				case HashMap.lookup nextDirPath imdNodes of
					Nothing           -> Nothing
					(Just imnode)     ->
						case imnode of
							(IMNodeDir imd) -> loop rest imd
							(IMNodeFile _)
								| null rest   -> Just VFile
								| otherwise   -> Nothing
	{-# INLINEABLE vfsTypeC #-}

	vfsContentsC = awaitForever $ \filepath -> do
			result <- lift $ withIMVFSRootDir $ loop (splitPath filepath)
			case result of
				Nothing          -> return ()
				(Just resultLBS) -> yield (filepath, resultLBS)
		where
			loop :: [FilePath] -> IMDirectory -> m (Maybe ByteString)
			loop [] _ = return Nothing
			loop [filename] IMDirectory{imdNodes} =
				case HashMap.lookup filename imdNodes of
					Nothing                     -> return Nothing
					(Just (IMNodeDir _))        -> return Nothing
					(Just (IMNodeFile fileData)) ->
						case fileData of
							(Resident bytes)    -> return (Just bytes)
							EmptyFile           -> return (Just mempty)
			loop (dirname:rest) IMDirectory{imdNodes} =
				case HashMap.lookup dirname imdNodes of
					(Just (IMNodeDir imd)) -> loop rest imd
					_                      -> return Nothing
	{-# INLINEABLE vfsContentsC #-}

	vfsContentsEitherC = awaitForever $ \filepath -> do
			maybeResult <- lift $ withIMVFSRootDir $ loop (splitPath filepath)
			case maybeResult of
				Nothing -> return ()
				(Just bytes) -> do
					yield $ Left filepath
					yield $ Right bytes
		where
			loop :: [FilePath] -> IMDirectory -> m (Maybe ByteString)
			loop [] _ = return Nothing
			loop [filename] IMDirectory{imdNodes} =
				case HashMap.lookup filename imdNodes of
					Nothing                      -> return Nothing
					(Just (IMNodeDir _))         -> return Nothing
					(Just (IMNodeFile filedata)) ->
						case filedata of
							(Resident bytes)    -> return $ Just bytes
							EmptyFile           -> return $ Just mempty
			loop (dirname:rest) IMDirectory{imdNodes} =
				case HashMap.lookup dirname imdNodes of
					(Just (IMNodeDir imd)) -> loop rest imd
					_                      -> return Nothing
	{-# INLINEABLE vfsContentsEitherC #-}

	vfsChildrenC = awaitForever $ \filepath ->
			lift ( withIMVFSRootDir $ return . loop filepath (splitPath filepath) ) >>= yieldMany
		where
			loop :: FilePath -> [FilePath] -> IMDirectory -> [FilePath]
			loop _ [] IMDirectory{imdNodes} = HashMap.keys imdNodes
			loop filepath (foo:rest) IMDirectory{imdNodes} =
				case HashMap.lookup foo imdNodes of
					(Just (IMNodeDir imd@(IMDirectory dir)))
						| null rest          -> (filepath </>) <$> HashMap.keys dir
						| otherwise          -> loop filepath rest imd
					_                      -> mempty
	{-# INLINEABLE vfsChildrenC #-}

-- | A class denoting that the type is usable as VFS conduits for writing.
instance (MonadUnliftIO m) => WriteVFSC (InMemoryVFS m) where

	vfsWriteEitherSink = awaitForever $ \case
		(Right _) -> return () -- Ignore: bytes without a file they belong to!
		(Left filepath) -> do
			imfile <- withSystemTempFile "imvfsc.tmp" $ \tmpFilePath tmpHandle -> do
				liftIO $ hSetBuffering tmpHandle $ BlockBuffering Nothing
				liftIO $ hSetBinaryMode tmpHandle True
				writeBytes tmpHandle
				fileSize <- liftIO $ hFileSize tmpHandle
				hClose tmpHandle
				if fileSize == 0 then
					return EmptyFile
				else
					liftIO $ Resident <$> withBinaryFile tmpFilePath ReadMode LBS.hGetContents
			lift $ modifyIMVFSRootDir $ return . loop (IMNodeFile imfile) (splitPath filepath)
		where
			loop _ [] imd = imd
			loop node [filename] imd@IMDirectory{imdNodes} = imd { imdNodes = HashMap.insert filename node imdNodes }
			loop node (name:rest) imd@IMDirectory{imdNodes} = imd { imdNodes = HashMap.alter
				(\case
					Nothing                     -> Just . IMNodeDir . loop node rest $ IMDirectory { imdNodes=mempty }
					(Just (IMNodeDir childImd)) -> Just . IMNodeDir $ loop node rest childImd
					whatever                    -> whatever
				) name imdNodes }
			writeBytes handle = do
				nextInput <- peekC
				let haveMoreBytes = case nextInput of
					(Just (Right _)) -> True
					_                -> False
				when haveMoreBytes $ do
					moreBytes <- await
					case moreBytes of
						(Just (Right bytes)) -> liftIO $ hPut handle (LBS.toStrict bytes)
						_                    -> return ()
	{-# INLINEABLE vfsWriteEitherSink #-}

	vfsRemoveSink = awaitForever $ \filepath -> lift . modifyIMVFSRootDir $ return . loop (splitPath filepath)
		where
			loop [] imd = imd
			loop [filename] imd@IMDirectory{imdNodes} = imd { imdNodes = HashMap.delete filename imdNodes }
			loop (name:rest) imd@IMDirectory{imdNodes} = imd
				{ imdNodes = HashMap.adjust
					(\case
						(IMNodeDir childImd) -> IMNodeDir $ loop rest childImd
						whatever             -> whatever
					) name imdNodes
				}
	{-# INLINE vfsRemoveSink #-}

instance (MonadUnliftIO m) => VFSC (InMemoryVFS m)
