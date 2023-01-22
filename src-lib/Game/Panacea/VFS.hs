{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Game.Panacea.VFS ( Header(..), VFS, allPaths, allFiles, empty, value, name, subdirCount, fileCount, fileData, resolve, isFile, isDir ) where

import qualified Data.ByteString as BS
import Data.List.Split ( splitOn )
import Data.Word ( Word32, Word64 )
import Game.Panacea.Internal.Tree hiding ( value, resolve )
import qualified Game.Panacea.Internal.Tree as T

data Header = RootDirHeader  !Word32 !Word32 
            | SubDirHeader   !String !Word32 !Word32
            | FileHeader     !String !Word32 !Word32 !Word64
            | FileHeaderFull !String !Word32 !Word32 !Word64 !BS.ByteString
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
name (FileHeaderFull fileName _ _ _ _) = fileName

subdirCount :: Header -> Word32
subdirCount (RootDirHeader numSubdirs _) = numSubdirs
subdirCount (SubDirHeader _ numSubdirs _) = numSubdirs

fileCount :: Header -> Word32
fileCount (RootDirHeader _ numFiles) = numFiles
fileCount (SubDirHeader _ _ numFiles) = numFiles

fileData :: Header -> BS.ByteString
fileData (FileHeaderFull _ _ _ _ theData) = theData

isFile :: Header -> Bool
isFile (FileHeader {}) = True
isFile (FileHeaderFull {}) = True
isFile _ = False

isDir :: Header -> Bool
isDir = not . isFile


resolve :: String -> VFS -> Maybe Header
resolve stringPath vfs = T.resolveBy (\ele header -> (==ele) . name $ header) (splitOn "/" stringPath) (left vfs) >>= T.value

allFiles :: VFS -> [([String],Header)]
allFiles = filter (isFile . snd) . allPaths

allPaths :: VFS -> [([String],Header)]
allPaths = allPaths' [] . left

allPaths' :: [String] -> VFS -> [([String],Header)]
allPaths' _ Tip = []
allPaths' parent (Branch l x r) = (parent,x):(allPaths' (parent++[name x]) l ++ allPaths' parent r)
