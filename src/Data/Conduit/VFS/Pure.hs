{-|
Description: VFS persisted purely through 'StateT'

This is intended primarily to be a drop-in testing mock for VFS implementations, but you may also find it useful within your program if you want to
work with some hierarchically-organized data.

Individual read and write operations are atomic.

The 'FilePath' values used in this VFS are split using 'splitPath' and joined using '</>', but are otherwise used directly: there is no concept of
paths being "relative" or "absolute" for this VFS.  It is also possible for a file and a directory to have the same name, since directories names
are appended with @/@, as per 'splitPath'.  (This implementation detail is up for debate and may be changed in a future major release: please file
an issue if you want to have a discussion around it.)

-}
module Data.Conduit.VFS.Pure
	( PureVFS
	, runPureVFS
	, runPureVFS'
	, runPureVFS_
	) where

import Control.Monad.Trans.State.Lazy
import ClassyPrelude hiding (ByteString, handle, hash)
import Data.Conduit.VFS.Import
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)
import Control.Monad.Extra (whenJust, maybeM)
import Control.Monad.Fail (MonadFail)
import System.FilePath (splitPath, (</>))
import qualified Data.Text as Text

-- | The possible kinds of nodes
data PNode
	= PNodeDir (HashMap Text PNode) -- ^ A directory
	| PNodeFile ByteString          -- ^ A file

-- | The basic implementation of the VFS.
newtype PureVFS m a = PureVFS
	{ unPVFS :: StateT PNode m a }
	deriving (Applicative, Functor, MonadFail, Monad, MonadTrans)

-- | Given a 'PureVFS', run it in the local monad and return both the monadic return value and the root node of the VFS.
runPureVFS :: PureVFS m a -> m (a, PNode)
runPureVFS pvfs = runStateT (unPVFS pvfs) (PNodeDir mempty)
{-# INLINE runPureVFS #-}

-- | Given a 'PureVFS', run it in the local monad and return the root node of the VFS.
runPureVFS' :: (Monad m) => PureVFS m a -> m PNode
runPureVFS' pvfs = execStateT (unPVFS pvfs) (PNodeDir mempty)
{-# INLINE runPureVFS' #-}

-- | Given a 'PureVFS', run it in the local monad and disregard any results.
runPureVFS_ :: (Monad m) => PureVFS m a -> m ()
runPureVFS_ = void . runPureVFS
{-# INLINE runPureVFS_ #-}

-- | Retrieves the root of the 'PureVFS'.
getRoot :: (Monad m) => PureVFS m PNode
getRoot = PureVFS get
{-# INLINE getRoot #-}

-- | Sets the root of the 'PureVFS' and returns the new root
setRoot :: (Monad m) => PNode -> PureVFS m PNode
setRoot newRoot = setRoot_ newRoot >> return newRoot
{-# INLINE setRoot #-}

-- | Sets the root of the 'PureVFS' without returning it
setRoot_ :: (Monad m) => PNode -> PureVFS m ()
setRoot_ = PureVFS . put
{-# INLINE setRoot_ #-}

-- | Changes the root without providing any return value
modifyRoot_ :: (Monad m) => (PNode -> PureVFS m PNode) -> PureVFS m ()
modifyRoot_ f = getRoot >>= f >>= PureVFS . put -- Oh, right: StateT doesn't have concurrency problems.  Sweet!
{-# INLINE modifyRoot_ #-}

-- | Changes the root and provides the updated value.
modifyRoot :: (Monad m) => (PNode -> PureVFS m PNode) -> PureVFS m PNode
modifyRoot f = modifyRoot_ f >> getRoot
{-# INLINE modifyRoot #-}

getNodeC :: (Monad m) => FilePath -> ConduitT i o (PureVFS m) (Maybe PNode)
getNodeC = lift . getNode
{-# INLINE getNodeC #-}

getNode :: (Monad m) => FilePath -> PureVFS m (Maybe PNode)
getNode filepath = loop (splitPath filepath) <$> getRoot
	where
		loop [] _ = Nothing
		loop _ (PNodeFile _) = Nothing
		loop [filename] (PNodeDir hash) = HashMap.lookup (Text.pack filename) hash
		loop (dirname:rest) (PNodeDir hash) = HashMap.lookup (Text.pack dirname) hash >>= loop rest
{-# INLINE getNode #-}

modifyNodeC_ :: (Monad m) => FilePath -> (Maybe PNode -> PureVFS m (Maybe PNode)) -> ConduitT i o (PureVFS m) ()
modifyNodeC_ filepath f = lift $ modifyNode_ filepath f
{-# INLINE modifyNodeC_ #-}

modifyNode_ :: (Monad m) => FilePath -> (Maybe PNode -> PureVFS m (Maybe PNode)) -> PureVFS m ()
modifyNode_ filepath f = modifyRoot_ $ loop (splitPath filepath)
	where
		loop [] node = return node
		loop _ file@(PNodeFile _) = return file
		loop (nodename:rest) dir@(PNodeDir hash) = case HashMap.lookup (Text.pack nodename) hash of
			Nothing -> case rest of
				[] ->
					maybeM
						(return dir)
						(\result -> return . PNodeDir $ HashMap.insert (Text.pack nodename) result hash)
						(f (Just dir))
				(restHead:restTail) ->
					loop restTail (PNodeDir mempty) >>= \result ->
						return . PNodeDir $ HashMap.insert (Text.pack nodename) (PNodeDir $ HashMap.singleton (Text.pack restHead) result) hash
			(Just node) -> loop rest node >>= \result -> return $ PNodeDir $ HashMap.insert (Text.pack nodename) result hash
{-# INLINEABLE modifyNode_ #-}

modifyNodeC :: (Monad m) => FilePath -> (Maybe PNode -> PureVFS m (Maybe PNode)) -> ConduitT i o (PureVFS m) (Maybe PNode)
modifyNodeC filepath f = lift $ modifyNode filepath f
{-# INLINE modifyNodeC #-}

-- | Updates the node at the given filepath and then returns the updated node.
modifyNode :: (Monad m) => FilePath -> (Maybe PNode -> PureVFS m (Maybe PNode)) -> PureVFS m (Maybe PNode)
modifyNode filepath f = modifyNode_ filepath f >> getNode filepath
{-# INLINE modifyNode #-}

instance (Monad m) => ReadVFSC (PureVFS m) where

	vfsTypeC = awaitForever $ \filepath -> do
			maybeNode <- getNodeC filepath
			yield (filepath, toType <$> maybeNode)
		where
			toType (PNodeDir _) = VDirectory
			toType (PNodeFile _) = VFile
	{-# INLINE vfsTypeC #-}

	vfsContentsC = awaitForever $ \filepath -> do
		maybeResult <- getNodeC filepath
		whenJust maybeResult $ \case
			(PNodeFile bytes) -> yield (filepath, bytes)
			(PNodeDir _)      -> return ()
	{-# INLINE vfsContentsC #-}

	vfsChildrenC = awaitForever $ \filepath -> do
		maybeNode <- getNodeC filepath
		case maybeNode of
			Nothing -> yield filepath
			(Just (PNodeFile _)) -> yield filepath
			(Just (PNodeDir hash)) -> yieldMany $ (filepath </>) . Text.unpack <$> HashMap.keys hash
	{-# INLINE vfsChildrenC #-}

-- | A class denoting that the type is usable as VFS conduits for writing.
instance (Monad m) => WriteVFSC (PureVFS m) where

	vfsWriteSink = awaitForever $ \(filepath, bs) -> modifyNodeC_ filepath (const . return . Just $ PNodeFile bs)
	{-# INLINE vfsWriteSink #-}

	vfsRemoveSink = awaitForever $ flip modifyNodeC_ (const $ return Nothing)
	{-# INLINE vfsRemoveSink #-}

-- | A class denoting that the type is usable as VFS conduits for reading and writing.
instance (Monad m) => VFSC (PureVFS m)
