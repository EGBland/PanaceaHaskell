{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Game.Panacea.VFS ( Header(..), VFS, empty, value, name, subdirCount, fileCount ) where

import Data.Word ( Word32, Word64 )
import Game.Panacea.Internal.Tree hiding ( value )
import qualified Game.Panacea.Internal.Tree as T

data Header = RootDirHeader !Word32 !Word32 
            | SubDirHeader  !String !Word32 !Word32
            | FileHeader    !String !Word32 !Word32 !Word64
            deriving (Show, Eq, Read)

type VFS = Tree Header

empty :: VFS
empty = Tip

value :: VFS -> Maybe Header
value = T.value

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