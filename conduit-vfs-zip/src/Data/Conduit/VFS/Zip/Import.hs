module Data.Conduit.VFS.Zip.Import (
   createDiskZipVFS,
   emptyZipVFS,
   createZipVFS
) where

import ClassyPrelude hiding (finally)
import Data.Conduit.VFS.Zip.Types
import Conduit
import Control.Monad.State.Lazy
import Codec.Archive.Zip (Archive, toArchive, fromArchive, emptyArchive)
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Trans.Resource (allocate)
import UnliftIO.Directory (doesFileExist)
import Control.Monad.Extra (ifM)

-- | Creates a Zip VFSC backed by a file. The file will be loaded in the resulting monad, and the resulting archive will be written back to the file path
--   when the conduit completes. If the file does not exist when the monad is initially executed, then an empty archive is used initially and the zip
--   file will be created at the given filepath when the conduit completes.
createDiskZipVFS :: (MonadResource m) => FilePath -> m (ZipVFS m Archive)
createDiskZipVFS archiveFilePath = createZipVFS . snd <$> allocate readArchive writeArchive
   where
    readArchive =
      ifM
         (doesFileExist archiveFilePath)
         (toArchive <$> LBS.readFile archiveFilePath)
         (return emptyArchive)
    writeArchive = LBS.writeFile archiveFilePath . fromArchive
{-# INLINEABLE createDiskZipVFS #-}

-- | Creates a Zip VFSC without any entries, and not attached to any file.
emptyZipVFS :: (Applicative m) => ZipVFS m Archive
emptyZipVFS = createZipVFS emptyArchive
{-# INLINE emptyZipVFS #-}

-- | Creates a Zip VFSC based on the provided 'Archive', and not attached to any file.
createZipVFS :: (Applicative m) => Archive -> ZipVFS m Archive
createZipVFS archive = ZipVFS $ StateT stateImpl
    where
        stateImpl = const $ pure (archive, archive)
{-# INLINE createZipVFS #-}
