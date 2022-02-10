module VFS (
    extractVFS, arrangeVFS,
    resolvePath
)
where

import Prelude hiding (writeFile)

import Data.Binary (get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (writeFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.UTF8 (toString)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust, isNothing)
import Data.Word
import System.Directory (createDirectory)
import Text.Printf (printf)

import Tree
import VFS.Get
import VFS.Prelude
import VFS.Util

-- determine if a given header is a file
isFile :: Header -> Bool
isFile (FileHeader _ _ _ _) = True
isFile _ = False

-- determine if a given header is a directory
isDir :: Header -> Bool
isDir = not . isFile


------------------- DIR STUFF -------------------
resolvePath :: String -> VFS -> Maybe Header
resolvePath path (Branch left (RootDirHeader _ _) _) = resolvePath path left
resolvePath path root =
    let
        pathSegments = split '/' path
        pathResolver :: String -> Header -> Bool
        pathResolver n = (==n) . name
    in
        getNodeValue <$> resolveBy pathResolver pathSegments root

extractVFS :: BSL.ByteString -> String -> VFS -> IO ()
extractVFS vfsData path (Branch left (RootDirHeader _ _) _) = extractVFS vfsData path left
extractVFS vfsData path (Branch left (SubDirHeader name _ _) right) =
    do
        let subdirPath = printf "%s/%s" path name
        createDirectory subdirPath
        extractVFS vfsData subdirPath left
        extractVFS vfsData path right

extractVFS vfsData path (Branch _ (FileHeader name size offset time) right) =
    do
        let filePath = printf "%s/%s" path name
        let fileData = runGet (getFileData $ FileHeader name size offset time) vfsData
        writeFile filePath fileData
        extractVFS vfsData path right

extractVFS _ _ Tip = return ()

(£+) :: (Num a) => (a,a) -> (a,a) -> (a,a)
(x1,y1) £+ (x2,y2) = (x1+x2,y1+y2)

countLevel :: VFS -> (Word32,Word32)
countLevel (Branch child (RootDirHeader _ _) _) = countLevel' child
countLevel (Branch child (SubDirHeader _ _ _) _) = countLevel' child

countLevel' :: VFS -> (Word32,Word32)
countLevel' (Branch _ (FileHeader _ _ _ _) sib) = (0,1) £+ countLevel' sib
countLevel' (Branch _ (SubDirHeader _ _ _) sib) = (1,0) £+ countLevel' sib
countLevel' Tip = (0,0)

arrangeVFS :: VFS -> VFS
arrangeVFS vfs = arrangeVFS' (getHeaderSize vfs) vfs

arrangeVFS' :: Int -> VFS -> VFS
arrangeVFS' _ (Branch left (RootDirHeader sdct fct) right) =
    let
        (sdct2,fct2) = countLevel $ Branch left (RootDirHeader sdct fct) right
    in
        Branch left (RootDirHeader sdct2 fct2) right

--arrangeVFS' offset (Branch left (FileHeader name size _ time) right) = 

getHeaderSize :: VFS -> Int
getHeaderSize =
    let
        f :: Header -> Int -> Int
        f (RootDirHeader _ _) = (+12)
        f (SubDirHeader name _ _) = (+9) . (+length name)
        f (FileHeader name _ _ _) = (+17) . (+length name)
    in
        foldr f 0


countTree :: VFS -> (Int,Int)
countTree vfs =
    let
        f :: Header -> (Int,Int) -> (Int,Int)
        f (RootDirHeader sdct fct) (d,f) = (d+fromEnum sdct,f+fromEnum fct)
        f (SubDirHeader _ sdct fct) (d,f) = (d+fromEnum sdct,f+fromEnum fct)
        f _ (d,f) = (d,f)
    in
        foldr f (0,0) vfs
