module VFS.Prelude (
    Header (..), VFS,
    name, subdirCount, fileCount
)
where

import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)

import Tree (Node)

data Header =
    RootDirHeader  !Word32 !Word32 |
    SubDirHeader   !String !Word32 !Word32 |
    FileHeader     !String !Word32 !Word32 !Word64 |
    FileHeaderFull !String !Word32 !Word32 !BS.ByteString
    deriving (Show)
type VFS = Node Header

name :: Header -> String
name (RootDirHeader _ _) = "<root>"
name (SubDirHeader name _ _) = name
name (FileHeader name _ _ _) = name

subdirCount :: Header -> Word32
subdirCount (RootDirHeader sdct _) = sdct
subdirCount (SubDirHeader _ sdct _) = sdct

fileCount :: Header -> Word32
fileCount (RootDirHeader _ fct) = fct
fileCount (SubDirHeader _ _ fct) = fct