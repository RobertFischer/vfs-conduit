{-|
Description: VFS interface to the local filesystem (conventionally but increasingly wrongly called "writing to disk").

Read operations are not atomic, but write operations are probably atomic. Specifically, reads acquire a 'SharedLock' via 'hLock'. Writes first persist to a
temporary file, and then perform a copy using 'copyFileWithMetadata'. If 'copyFileWithMetadata' is atomic on your implementation (it probably is), then
writes are atomic.

The 'FilePath' values used in this VFS are split using 'splitPath' and joined using '</>'. Relative paths are resolved relative to the current working
directory: changing that directory is outside the scope of this module.

-}
module Data.Conduit.VFS.Disk
	( DiskVFS
	, runDiskVFS
	, runDiskVFS_
	) where


import ClassyPrelude hiding (ByteString, handle, hash, bracket)
import Control.Monad.Extra (ifM)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Loops (whileM_)
import Data.Conduit.VFS.Import
import System.IO.Extra (openBinaryFile)
import System.Posix (getFileStatus, isRegularFile, isDirectory)
import System.Directory (removeFile)
import UnliftIO.Directory (doesFileExist, listDirectory)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS

-- | The basic implementation of the VFS.
newtype DiskVFS m a = DiskVFS { unDVFS :: m a }
	deriving (Applicative, Functor, MonadFail, Monad)

instance (MonadIO m) => MonadIO (DiskVFS m) where
	liftIO = DiskVFS . liftIO
	{-# INLINE liftIO #-}

instance (MonadUnliftIO m) => MonadUnliftIO (DiskVFS m) where
	askUnliftIO = do
		(UnliftIO interiorUnliftIO) <- lift askUnliftIO
		return $ UnliftIO $ \(DiskVFS interior) -> interiorUnliftIO interior
	{-# INLINE askUnliftIO #-}

instance MonadTrans DiskVFS where
	lift = DiskVFS
	{-# INLINE lift #-}

-- | Given a 'DiskVFS', run it in the local monad and return the monadic return value.
runDiskVFS :: DiskVFS m a -> m a
runDiskVFS = unDVFS
{-# INLINE runDiskVFS #-}

-- | Given a 'DiskVFS', run it in the local monad and disregard any results.
runDiskVFS_ :: (Monad m) => DiskVFS m a -> m ()
runDiskVFS_ = void . runDiskVFS
{-# INLINE runDiskVFS_ #-}

instance (MonadUnliftIO m) => ReadVFSC (DiskVFS m) where

	vfsTypeC = awaitForever $ \filepath -> fmap (filepath,) . liftIO $
		ifM
			(not <$> doesFileExist filepath)
			(return Nothing)
			$ getFileStatus filepath >>= \status ->
				if isRegularFile status then
					return $ Just VFile
				else if isDirectory status then
					return $ Just VDirectory
				else
					return Nothing
	{-# INLINEABLE vfsTypeC #-}

	vfsContentsEitherC = awaitForever $ \filepath ->
			whenM (isExistingRegularFile filepath)
				$ do
					yield $ Left filepath
					handle <- liftIO $ openBinaryFile filepath ReadMode
					liftIO $ hSetBuffering handle (BlockBuffering Nothing)
					whileM_
						(hIsNotEOF handle)
						(doRead handle >>= yield . Right . LBS.fromStrict)
		where
			hIsNotEOF handle = liftIO $ not <$> hIsEOF handle
			doRead h = liftIO $ SBS.hGetSome h 1024
	{-# INLINEABLE vfsContentsEitherC #-}

	vfsChildrenC = awaitForever $ \filepath ->
			whenM (liftIO $ doesFileExist filepath) $
				ifM
					(fileIsDirectory filepath)
					(listChildren filepath >>= yieldMany)
					(yield filepath)
		where
			fileIsDirectory filepath = liftIO $ isDirectory <$> getFileStatus filepath
			listChildren filepath = liftIO $ do
				(children::[FilePath]) <- listDirectory filepath
				return $ (filepath </>) <$> children
	{-# INLINEABLE vfsChildrenC #-}

-- | A class denoting that the type is usable as VFS conduits for writing.
instance (MonadUnliftIO m) => WriteVFSC (DiskVFS m) where

	vfsWriteEitherSink = awaitForever $ \case
			(Right _) ->  fail "Encountered bytes without seeing a filename"
			(Left filename) -> do
				bytes <- readAllBytesFromUpstream
				liftIO $ LBS.writeFile filename bytes
		where
			readAllBytesFromUpstream =
				ifM
					moreBytesFromUpstream
					(readSomeBytesFromUpstream >>= \prev -> LBS.append prev <$> readAllBytesFromUpstream )
					(return mempty)
			moreBytesFromUpstream = peekC >>= \case
					(Just (Right _)) -> return True
					_ -> return False
			readSomeBytesFromUpstream = await >>= \case
					(Just (Right bytes)) -> return bytes
					_ -> fail "Encountered a new filename when peeking said we had bytes"
	{-# INLINEABLE vfsWriteEitherSink #-}

	vfsRemoveSink = awaitForever $ \filename ->
			whenM (isExistingRegularFile filename) (liftIO $ removeFile filename)
	{-# INLINE vfsRemoveSink #-}

isExistingRegularFile :: MonadIO m => FilePath -> m Bool
isExistingRegularFile filepath = liftIO $ liftM2 (&&) (doesFileExist filepath) (isRegularFile <$> getFileStatus filepath)

-- | A class denoting that the type is usable as VFS conduits for reading and writing.
instance (MonadUnliftIO m) => VFSC (DiskVFS m)

