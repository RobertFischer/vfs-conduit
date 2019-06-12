{- | Description: Treats zip files like a virtual file system.

  This module provides two virtual file system implementations: 'ZipVFS' and 'DiskZipsVFS'.  The 'ZipVFS' implementation represents a zip archive as a self-contained
  and in-memory filesystem.  The 'DiskZipsVFS' is akin to the disk VFS, except that it knows how to read and write zip entries, so the entries internal to zip files
  work the same way.

  In both systems, compression within the archive is automatic: we attempt compression, and if the result saves us some bytes, then we used the compressed value; otherwise,
  we simply store the bytes uncompressed.

 -}

module Data.Conduit.VFS.Zip (
    module Data.Conduit.VFS.Zip.Types,
    module Data.Conduit.VFS.Zip.Import,
    module Data.Conduit.VFS.Types
) where

import Data.Conduit.VFS.Zip.Types
import Data.Conduit.VFS.Zip.Import
import Data.Conduit.VFS.Types
