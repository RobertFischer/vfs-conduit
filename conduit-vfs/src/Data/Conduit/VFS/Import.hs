{-|

Description: The common imports for this library's files.

-}

module Data.Conduit.VFS.Import
( module Data.Conduit.VFS.Types
, module Conduit
, (&)
) where

import ClassyPrelude
import Data.Conduit.VFS.Types
import Conduit

(&) :: a -> (a -> b) -> b
(&) a f = f a
