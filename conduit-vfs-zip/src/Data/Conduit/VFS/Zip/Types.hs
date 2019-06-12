module Data.Conduit.VFS.Zip.Types (
    module Data.Conduit.VFS.Types,
    ZipVFS(..),
    DiskZipsVFS(..)
) where

import ClassyPrelude hiding (ByteString, finally)
import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry, filesInArchive, addEntryToArchive, toEntry, deleteEntryFromArchive, toArchive, emptyArchive, fromArchive)
import Conduit
import Control.Monad.Catch (MonadCatch, MonadMask, finally)
import Control.Monad.Extra (whenJust)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Lazy (StateT, put, get, MonadState, modify)
import Data.Conduit.VFS.Types
import Data.List.Extra (split)
import System.FilePath (pathSeparator, isPathSeparator, isExtensionOf, isSearchPathSeparator)
import qualified Data.ByteString.Lazy as LBS
import UnliftIO.Directory (doesFileExist, doesDirectoryExist, listDirectory, removeFile)

-- | Represents a single zip file as a conduit. Note that the zip file is resident in-memory as an 'Archive'. The 'Archive' type holds a list of 'Entry',
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

   vfsContentsC = awaitForever $ \rawFilepath -> get >>= \archive ->
      let filepath = unnormalize rawFilepath in
      whenJust (findEntryByPath filepath archive) $ \entry -> yield (filepath, fromEntry entry)
   {-# INLINEABLE vfsContentsC #-}

   vfsTypeC = awaitForever $ \rawFilepath -> get >>= \archive ->
      let filepath = unnormalize rawFilepath in
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

   vfsDescendentsC = awaitForever $ \rawFilepath -> get >>= \archive ->
      let filepath = unnormalize rawFilepath in
      let isFile = isJust $ findEntryByPath filepath archive in
      yieldMany $
         if isFile then
            [filepath]
         else
            filter (`isPrefixOf` filepath) (filesInArchive archive)
   {-# INLINEABLE vfsDescendentsC  #-}

instance (Monad m) => WriteVFSC (ZipVFS m) where

   vfsWriteSink = awaitForever $ \(filepath, bytes) -> modify $
      addEntryToArchive (toEntry (unnormalize filepath) 0 bytes)
   {-# INLINEABLE vfsWriteSink #-}

   vfsRemoveSink = awaitForever $ \filepath -> modify $ deleteEntryFromArchive (unnormalize filepath)
   {-# INLINEABLE vfsRemoveSink #-}

instance (Monad m) => VFSC (ZipVFS m)

-- | The zip library that we are using assumes that paths are separated by forward slashes, so we have to
--   change the filepaths if the path separator generated by '</>' is not a forward slash. This is the
--   opposite of 'normalize'.
unnormalize :: FilePath -> FilePath
unnormalize filepath =
   if pathSeparator == '/' then
      filepath
   else
      intercalate "/" $ split isPathSeparator filepath
{-# INLINE unnormalize #-}


-- | This type is similar to 'DiskVFS', but it automatically expands the contents of zip files that it encounters. Zip files are files denoted by the file suffix ".zip".
--   The filepaths for internal entries are separated by the 'searchPathSeparator' character.
newtype DiskZipsVFS m a = DiskZipsVFS { unDiskZipsVFS :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadResource)

instance MonadTrans DiskZipsVFS where
   lift = DiskZipsVFS
   {-# INLINE lift #-}

instance (MonadUnliftIO m) => ReadVFSC (DiskZipsVFS m) where

   vfsTypeC = awaitForeverForZipFile' zipHandler restHandler
      where
         restHandler (filepath, content) = yield . (filepath,) $
            case content of
               (NoContent _)     -> Nothing
               (FileContent _ _) -> Just VFile
               (DirContent _ _)  -> Just VDirectory
         zipHandler (filepath, archive) = yield . (filepath,) $
            let (_, entryPath) = splitFilepath filepath in
            case findEntryByPath entryPath archive of
               (Just _) -> Just VFile
               Nothing ->
                  find (entryPath `isPrefixOf`) (filesInArchive archive) >> return VDirectory

   vfsContentsC = awaitForeverForZipFile' zipHandler restHandler
      where
         restHandler (filepath, content) =
            case content of
               (FileContent _ bytes) -> yield (filepath, bytes)
               _                     -> return ()
         zipHandler (filepath, archive) = yield (filepath, fromArchive archive)

   vfsDescendentsC = awaitForeverForZipFile' zipHandler restHandler
      where
         restHandler (filepath, content) =
            case content of
               (FileContent _ _)       -> yield filepath
               (DirContent _ children) -> yieldMany ( (filepath </>) <$> children )
               (NoContent _)           -> return ()
         zipHandler (filepath, archive) = yieldMany ( ((filepath <> [pathSeparator]) <>) <$> filesInArchive archive )


instance (MonadUnliftIO m) => WriteVFSC (DiskZipsVFS m) where

   vfsWriteSink = awaitForeverForZipFile fst zipHandler restHandler
      where
         restHandler ((filepath, bytestring), _) = liftIO $ LBS.writeFile filepath bytestring
         zipHandler ((filepath, bytestring), archive) =
            let (archivePath, entryPath) = splitFilepath filepath in
            let archiveBytes =
                  if null entryPath then
                     bytestring
                  else
                     fromArchive $ addEntryToArchive (toEntry entryPath 0 bytestring) archive
            in
            transPipe DiskZipsVFS $ restHandler ( (archivePath, archiveBytes), FileContent archivePath archiveBytes)

   vfsRemoveSink = awaitForeverForZipFile' zipHandler restHandler
      where
         restHandler (filepath, _) = liftIO $ removeFile filepath
         zipHandler (filepath, oldArchive) =
            let (archivePath, entryPath) = splitFilepath filepath in
            if null entryPath then
               transPipe DiskZipsVFS $ restHandler ( archivePath, FileContent archivePath mempty)
            else
               let newArchive = deleteEntryFromArchive entryPath oldArchive in
               liftIO $ LBS.writeFile archivePath (fromArchive newArchive)

instance (MonadUnliftIO m) => VFSC (DiskZipsVFS m)

-- | Represents the content of a file, which may be nested super deeply somewhere.
data Content
   = FileContent FilePath ByteString  -- ^ A file with its contents as a lazy byte string
   | DirContent FilePath [FilePath]   -- ^ A directory with its immediate children
   | NoContent FilePath               -- ^ A node that does not exist

-- | Performs an 'awaitForever', but processes differently depending on whether or not the filepath has ".zip" as its file extension.
awaitForeverForZipFile :: (MonadIO m)
   => (i -> FilePath)                                          -- ^ Function to convert inputs into a filepath
   -> ((i,Archive) -> ConduitT i o (DiskZipsVFS m) ())         -- ^ Function for handling zip files
   -> ((i,Content) -> ConduitT i o m ())                       -- ^ Function for handling non-zip files
   -> ConduitT i o (DiskZipsVFS m) ()
awaitForeverForZipFile toFilePath zipHandler restHandlerBase = awaitForever $ \input ->
      let filepath = toFilePath input in
      let (archiveFilePath, entryFilePath) = splitFilepath filepath in
      let isNested = not $ null entryFilePath in
      if "zip" `isExtensionOf` filepath then
         readArchive archiveFilePath >>= \archive ->
            if isNested then
               case findEntryByPath entryFilePath archive of
                  Nothing -> return ()
                  (Just entry) -> zipHandler (input, toArchive $ fromEntry entry)
            else
               zipHandler (input, archive)
      else
         if isNested then
            readArchive archiveFilePath >>= \archive ->
               case findEntryByPath entryFilePath archive of
                  (Just entry) -> restHandler (input, FileContent archiveFilePath $ fromEntry entry)
                  Nothing ->
                     let children = filter (entryFilePath `isPrefixOf`) (filesInArchive archive) in
                     if null children then
                        return ()
                     else
                        restHandler (input, DirContent archiveFilePath children)
         else
            readContent filepath >>= \content -> restHandler (input, content)
   where
      restHandler = transPipe DiskZipsVFS . restHandlerBase
{-# INLINEABLE awaitForeverForZipFile #-}

-- | Performs an 'awaitForever', but processes differently depending on whether or not the filepath has ".zip" as its file extension. This version is for when
--   the input is just a 'FilePath': we can get rid of the first argument and specialize the types.
awaitForeverForZipFile' :: (MonadIO m)
   => ((FilePath, Archive) -> ConduitT FilePath o (DiskZipsVFS m) ())         -- ^ Function for handling zip files
   -> ((FilePath, Content) -> ConduitT FilePath o m ())                       -- ^ Function for handling non-zip files
   -> ConduitT FilePath o (DiskZipsVFS m) ()
awaitForeverForZipFile' = awaitForeverForZipFile id
{-# INLINE awaitForeverForZipFile' #-}

-- | Reads an archive, which may be nested inside another archive
readArchive :: MonadIO m => FilePath -> m Archive
readArchive filepath = do
      fileExists <- liftIO $ doesFileExist archiveFilePath
      if not fileExists then
         return emptyArchive
      else
         toArchive <$> liftIO (LBS.readFile archiveFilePath) >>= \archive ->
            return $
               if null entryFilePath then
                  archive
               else
                  case findEntryByPath entryFilePath archive of
                     Nothing -> archive
                     (Just entry) -> toArchive $ fromEntry entry
   where
      (archiveFilePath, entryFilePath) = splitFilepath filepath
{-# INLINEABLE readArchive #-}

-- | Reads a filepath and determines what kind of 'Content' it is. Note that this assumes filepath is
--   an actual file on the filesystem, not nested via 'searchPathSeparator'.
readContent :: MonadIO m => FilePath -> m Content
readContent filepath = liftIO $ do
      isFile <- doesFileExist filepath
      isDir <- liftM2 (&&) (return (not isFile)) (doesDirectoryExist filepath)
      case (isDir, isFile) of
         (False, False) -> return $ NoContent filepath
         (True, _)      -> DirContent filepath <$> liftIO (listDirectory filepath)
         (_, True)      -> FileContent filepath <$> liftIO (LBS.readFile filepath)
{-# INLINEABLE readContent #-}

-- | Splits the filepath at 'searchPathSeparator'
splitFilepath :: FilePath -> (FilePath, FilePath)
splitFilepath filepath = (archivePath, entryPath)
   where
      (archivePath, rawEntryPath) = break isSearchPathSeparator filepath
      entryPath = unnormalize rawEntryPath
{-# INLINE splitFilepath #-}