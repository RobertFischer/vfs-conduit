{-|

Description: The types that make up this library.

-}

module Data.Conduit.VFS.Types
( FilePath
, ConduitT
, VFSSource
, VFSPipe
, VFSSink
, VFileType(..)
, ReadVFSC(..)
, WriteVFSC(..)
, VFSC(..)
, ByteString
) where

import ClassyPrelude hiding (ByteString)
import System.FilePath (FilePath, takeDirectory)
import Conduit
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight)
import Control.Monad.Extra (ifM)

type ByteString = LBS.ByteString

-- | The type of conduits that generate file paths.
type VFSSource m = ConduitT Void FilePath m ()

-- | The type of conduits that consume file paths and generate file paths.
type VFSPipe m = ConduitT FilePath FilePath m ()

-- | The type of conduits that consume file paths.
type VFSSink m r = ConduitT FilePath Void m r

-- | The types that our virtual file system supports.
data VFileType
	= VFile        -- ^ A node containing bytes
	| VDirectory   -- ^ A node containing zero or more other nodes
	deriving (Eq, Ord, Show, Generic, Typeable, Enum, Bounded)

-- | A class denoting that the type is usable as VFS conduits for reading.
class (Monad m) => ReadVFSC m where

	{-# MINIMAL ( vfsContentsEitherC | vfsContentsC ), vfsTypeC, ( vfsChildrenC | vfsDescendentsC ) #-}

	-- | Given an input path, generates a tuple of the input path itself and the input path's 'VFileType' (or 'Nothing' if the node does not exist).
	--   Note that a directory containing no elements may be reported by the VFS as not existing.
	vfsTypeC :: ConduitT FilePath (FilePath, Maybe VFileType) m ()

	-- | Given an input of 'FilePath' files, generates a tuple containing the input and the bytestring for the contents of the file.
	--   Note that the entire contents of the file are pulled into memory.  If the input 'FilePath' does not denote a 'VFile', it should be dropped.
	vfsContentsC :: ConduitT FilePath (FilePath, LBS.ByteString) m ()
	vfsContentsC = awaitForever $ \path -> do
			bytes <- yield path .| vfsContentsEitherC .| mapC (fromRight mempty) .| foldC
			yield (path, bytes)
	{-# INLINEABLE vfsContentsC #-}

	-- | Given an input of 'FilePath' files, generates a 'Left' of the input, followed by zero or more 'Right' values holding a bytestring. The concatenation of the
	--   'Right' values after a given 'Left' and before the next 'Left' (or EOS) are the bytes of the input value. If the input 'FilePath' does not denote a 'VFile',
	--   it should be dropped.
	vfsContentsEitherC :: ConduitT FilePath (Either FilePath LBS.ByteString) m ()
	vfsContentsEitherC = awaitForever $ \path -> do
			yield $ Left path
			bytes <- yield path .| vfsContentsC .| mapC snd .| foldC
			yield $ Right bytes
	{-# INLINEABLE vfsContentsEitherC #-}

	-- | Given an input of 'FilePath' directories, generates the non-special direct children, each path-prepended (using '</>') with the parent directory.
	--   If an input 'FilePath' is not a 'VDirectory', it should be passed through directly.
	vfsChildrenC :: VFSPipe m
	vfsChildrenC = awaitForever $ \path -> do
			children <- yield path .| vfsDescendentsC .| filterC (\it -> path == takeDirectory it ) .| sinkList -- TODO: Is this a bug if we encounter paths like /foo/bar/foo?
			yieldMany children
	{-# INLINEABLE vfsChildrenC #-}

	-- | Given an input of 'FilePath' directories, generates the non-special direct children that are files, each path-prepended (using '</>') with the
	--   parent directory.  If an input 'FilePath' is not a 'VDirectory', it should be passed through directly.
	vfsChildFilesC :: VFSPipe m
	vfsChildFilesC = vfsChildrenC .| vfsTypeC .| filterC ( (Just VFile ==) . snd ) .| mapC fst
	{-# INLINE vfsChildFilesC #-}

	-- | Given an input of 'FilePath' directories, generates the non-special direct children that are files, each path-prepended (using '</>') with the
	--   parent directory.  If an input 'FilePath' is not a 'VDirectory', it should be dropped.
	vfsChildDirsC :: VFSPipe m
	vfsChildDirsC = vfsChildrenC .| vfsTypeC .| filterC ( (Just VDirectory ==) . snd ) .| mapC fst
	{-# INLINE vfsChildDirsC #-}

	-- | Given an input of 'FilePath' directories, generates all the paths in the VFS that have the input as a prefix, with the outputs being each
	--   path-prepended (using '</>') with the corresponding input directory. If an  input 'FilePath' is not a 'VDirectory', it should be passed through
	--   directly.
	vfsDescendentsC :: VFSPipe m
	vfsDescendentsC = awaitForever $ \path -> do
			yield path
			loop path
		where
			loop path = do
				children <- yield path .| vfsChildrenC .| sinkList
				yieldMany children
				unless (null children) (sequence_ $ loop <$> children)
	{-# INLINEABLE vfsDescendentsC #-}

	-- | Given an input 'FilePath' directories, generates all the paths in the VFS that are files and have the input as a prefix, with the outputs being
	--   each path-prepended (using '</>') with the corresponding input directory. If an input 'FilePath' is not a 'VDirectory', it should be passed through directly.
	vfsDescFilesC :: VFSPipe m
	vfsDescFilesC = vfsDescendentsC .| vfsTypeC .| filterC (\(_, maybeFileType) -> Just VFile == maybeFileType) .| mapC fst
	{-# INLINE vfsDescFilesC #-}

	-- | Given an input of 'FilePath' directories, generates all the paths in the VFS that are directories and have the input as a prefix, with the outputs being
	--   each path-prepended (using '</>') with the corresponding input directory. If an input 'FilePath' is not a 'VDirectory', it should be dropped.
	vfsDescDirsC :: VFSPipe m
	vfsDescDirsC = vfsDescendentsC .| vfsTypeC .| filterC ( (Just VDirectory ==) . snd ) .| mapC fst
	{-# INLINE vfsDescDirsC #-}

-- | A class denoting that the type is usable as VFS conduits for writing.
class (Monad m) => WriteVFSC m where

	{-# MINIMAL (vfsWriteSink | vfsWriteEitherSink), vfsRemoveSink #-}

	-- | Given an input tuple of 'FilePath' files and their bytestring contents, writes the contents to the filepath. This write should be atomic if possible, and if
	--   it is not an atomic operation, the implementation's documentation should make this clear. This write should also create any necessary directories that may
	--   not have previously existed.
	vfsWriteSink :: ConduitT (FilePath, LBS.ByteString) Void m ()
	vfsWriteSink = awaitForever $ \(filepath, bs) -> yieldMany [Left filepath, Right bs] .| vfsWriteEitherSink
	{-# INLINE vfsWriteSink #-}

	-- | Given an input of either 'FilePath' files or bytestring contents, writes the contents to the filepath. The write is marked as complete when the next
	--   'FilePath' input or end-of-stream is reached. This write should be atomic at completion if possible, and if it is not an atomic operation, the
	--   implementation's documentation should make this clear. This write should also create any necessary directories that may not have previously existed.
	vfsWriteEitherSink :: ConduitT (Either FilePath LBS.ByteString) Void m ()
	vfsWriteEitherSink = awaitForever $ \case
			(Right _) -> return () -- WTF?
			(Left filepath) -> do
				bytes <- bytesLoop
				yield (filepath, bytes) .| vfsWriteSink
		where
			bytesLoop = peekC >>= \case
				(Just (Right bytes)) -> await >> (bytes <>) <$> bytesLoop
				_                    -> return mempty
	{-# INLINEABLE vfsWriteEitherSink #-}

	-- | Given 'FilePath' inputs, remove those nodes from the VFS. If the path denotes a directory, the directory is
	--   removed along with all of its descendents. If the path denotes a file, the file itself is removed. After a removal,
	--   any newly-empty directories may also be removed.
	vfsRemoveSink :: VFSSink m ()

-- | A class denoting that the type is usable as VFS conduits for both reading and writing.
class (ReadVFSC m, WriteVFSC m) => VFSC m where

	-- | Given an input tuple of a filetype and a filepath, ensure that a node exists at the filepath. If it does not exist, it should be created as either
	--   a directory or a zero-length file, as denoted by the filetype, with any missing parent directories created.  Note that a directory which does not
	--   contain a node may be reported as not present by the VFS, and therefore an acceptable implementation for 'VDirectory' inputs is simply 'return ()'
	vfsTouchSink :: ConduitT (VFileType, FilePath) Void m ()
	vfsTouchSink = awaitForever $ \(filetype, filepath) ->
		ifM
			( yield filepath .| vfsTypeC .| headC >>= \case
				Nothing -> return False
				(Just (_, Nothing)) -> return False
				(Just (_, Just foundFileType)) -> return $ filetype == foundFileType
			)
			( case filetype of
					VFile -> yield (filepath, mempty) .| vfsWriteSink
					VDirectory -> return ()
			)
			( return () )
	{-# INLINEABLE vfsTouchSink #-}

