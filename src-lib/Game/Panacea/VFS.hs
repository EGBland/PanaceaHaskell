{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Game.Panacea.VFS ( Header(..), VFS, name, subdirCount, fileCount ) where

import Data.Word ( Word32, Word64 )
import Game.Panacea.Internal.Tree

data Header = RootDirHeader !Word32 !Word32 
            | SubDirHeader  !String !Word32 !Word32
            | FileHeader    !String !Word32 !Word32 !Word64
            deriving (Show, Eq, Read)

type VFS = Tree Header

name :: Header -> String
name (RootDirHeader _ _) = "<root>"
name (SubDirHeader subdirName _ _) = subdirName
name (FileHeader fileName _ _ _) = fileName

subdirCount :: Header -> Word32
subdirCount (RootDirHeader numSubdirs _) = numSubdirs
subdirCount (SubDirHeader _ numSubdirs _) = numSubdirs

fileCount :: Header -> Word32
fileCount (RootDirHeader _ numFiles) = numFiles
fileCount (SubDirHeader _ _ numFiles) = numFiles
