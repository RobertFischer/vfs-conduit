module Data.Conduit.VFS.Zip.Types (
    module Data.Conduit.VFS.Zip.Types,
    module Data.Conduit.VFS.Types
) where

import ClassyPrelude hiding (finally)
import Data.Conduit.VFS.Types
import Conduit
import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry, filesInArchive, addEntryToArchive, toEntry, deleteEntryFromArchive)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Catch (MonadCatch, MonadMask, finally)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Extra (whenJust)
import Control.Monad.State.Lazy (StateT, put, get, MonadState, modify)

-- | Represents a zip file as a conduit. Note that the zip file is resident in-memory as an 'Archive'. The 'Archive' type holds a list of 'Entry',
--   each of which holds their content as a lazy 'ByteString'. Because of this, you can use 'ZipVFS' as an alternative to 'PureVFS', which stores
--   its in-memory data as compressed bytes. It can also be used as an alternative to 'InMemoryVFS' as long as the conduit execution is single-threaded.
newtype ZipVFS m a = ZipVFS { unZipVFS :: StateT Archive m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadState Archive, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadResource)

instance (MonadMask m) => MonadReader Archive (ZipVFS m) where
    ask = get
    {-# INLINE ask #-}

    local f m = ask >>= \origState -> finally (runMonad origState) (put origState)
      where
        runMonad origState = put (f origState) >> m
    {-# INLINEABLE local #-}

instance (Monad m) => ReadVFSC (ZipVFS m) where

   vfsContentsC = awaitForever $ \filepath -> get >>= \archive ->
      whenJust (findEntryByPath filepath archive) $ \entry -> yield (filepath, fromEntry entry)
   {-# INLINEABLE vfsContentsC #-}

   vfsTypeC = awaitForever $ \filepath -> get >>= \archive ->
      let entryFilepaths = filesInArchive archive in
      let isFile = isJust $ find (filepath ==) entryFilepaths in
      let isDir = not isFile && isJust (find (`isPrefixOf` filepath) entryFilepaths) in
      yield . (filepath,) $
         if isFile then
            Just VFile
         else if isDir then
            Just VDirectory
         else
            Nothing
   {-# INLINEABLE vfsTypeC #-}

   vfsDescendentsC = awaitForever $ \filepath -> get >>= \archive ->
      let isFile = isJust $ findEntryByPath filepath archive in
      yieldMany $
         if isFile then
            [filepath]
         else
            filter (`isPrefixOf` filepath) (filesInArchive archive)
   {-# INLINEABLE vfsDescendentsC  #-}

instance (Monad m) => WriteVFSC (ZipVFS m) where

   vfsWriteSink = awaitForever $ \(filepath, bytes) -> modify $ addEntryToArchive (toEntry filepath 0 bytes)
   {-# INLINEABLE vfsWriteSink #-}

   vfsRemoveSink = awaitForever $ \filepath -> modify $ deleteEntryFromArchive filepath
   {-# INLINEABLE vfsRemoveSink #-}

instance (Monad m) => VFSC (ZipVFS m)
