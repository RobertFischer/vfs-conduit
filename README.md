Virtual File System (VFS) Conduit for Haskell
===============================================

Motivation
-------------

I was writing a Haskell program to process a _lot_ of JSON-formatted log records: far too many to hold in memory.
This is a perfect situation for [conduits](https://hackage.haskell.org/package/conduit), so I built out my pipeline. However, I encountered a bit of a hitch: many of the
files were stored in `.tar.gz` formats, so what I wanted was to recursively navigate into the `.tar.gz` file and feed its embedded files into my pipeline. What would be even
cooler is if I could slurp in S3 files, or files from Dropbox, or any of that, and then process them all the same way.

Concepts
----------------

This library is an abstraction of a _filesystem_ that operates via conduits. A _filesystem_ is defined as a particular kind of key-value store. The keys are hierarchical strings,
with different levels of the hierarchy being seperated by [`</>`](https://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath-Posix.html#v:-60--47--62-). These
levels are referred to as _directories_. Directories may be listed and navigated, but are neither directly created nor directly destroyed. The leaf values are lazy
[`ByteString`](https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy.html) values. These values are referred to as _files_. Files can be listed,
removed, and written. A file's _path_ consists of the names of its parent directories, from most distant to immediate, concatenated by `</>`.  The directories and files
of a filesystem are that filesystem's _nodes_. A directory that contains no child nodes (ie: an empty directory) may or may not be deleted, which may cause recursive
deletes of ancestor directories if the ancestor is now empty. When a file is written, any missing directories in the file's path will be created.

Provided Virtual File Systems
----------------------------------

There are three virtual file system implementations provided as a part of this library:

	* `InMemory` -- This filesystem uses an [`MVar`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html) to store the state of the filesystem
		in memory. All the data for all the files are currently stored in-memory as the raw bytes: future versions of this library may persist larger files into compressed
		bytestrings or temp files.
	* `Pure` -- This filesystem uses [`StateT`](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-State-Lazy.html#t:StateT) to persist the state
		of the filesystem. Unlike `InMemory`, the state is not portable and changes in state are not persisted after the monad resolves. However, this implementation is pure,
		which means it can be used with pure conduits or as a way to make testing faster and idempotent.
	* `Disk` -- This virtual file system is the traditional disk-based filesystem, with folders persisted to the operating system's filesystem.

Other Virtual File Systems
-------------------------------

There are other VFS implementations in the works:

	* `Zip` -- Treat a `.zip` file as a filesystem, and provide conduits that will auto-expand zip entries provided by upstream VFS conduits.
	* `Tar` -- Treat a `.tar` file as a filesystem, and provide conduits that will auto-expand tar entries provided by upstream VFS conduits.
	* `S3` -- Treat an AWS S3 bucket as a filesystem.

In addition, we are looking to implement a conduit which automatically uncompresses or compresses `.xz`, `.lzma`, `.z`, and `.gz` files provided by upstream VFS conduits.
