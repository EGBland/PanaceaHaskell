module VFS.Get (
    getVFS, getFileData
)
where

import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.UTF8 (toString)
import Data.Word

import VFS.Prelude
import Tree

-- |Get a string with 8-bit length
getVFSStr :: Get String
getVFSStr = do
    strlen <- getWord8
    str <- getByteString $ fromEnum strlen
    return . toString $ str

-- |Get root directory header (will error if the magic number doesn't align)
getRootDirHeader :: Get Header
getRootDirHeader = getWord32le >>= getRootDirHeader'

getRootDirHeader' :: Word32 -> Get Header
getRootDirHeader' 0x4331504C = do
    subdir_ct <- getWord32le
    file_ct   <- getWord32le
    return (RootDirHeader subdir_ct file_ct)

-- |Get header for subdirectory, which occurs after the file list of a directory
getSubDirHeader :: Get Header
getSubDirHeader = do
    name <- getVFSStr
    subdir_ct <- getWord32le
    file_ct <- getWord32le
    return (SubDirHeader name subdir_ct file_ct)

-- |Get root directory as a VFS
getRootDir :: Get VFS
getRootDir = getRootDirHeader >>= (return . stub)

getSubDir :: Get VFS
getSubDir = getSubDirHeader >>= (return . stub)

getFile :: Get VFS
getFile = do
    name <- getVFSStr
    size <- getWord32le
    off  <- getWord32le
    time <- getWord64le
    return . stub $ FileHeader name size off time

getVFS :: Get VFS
getVFS = getRootDir >>= getVFS'

getVFS' :: VFS -> Get VFS
getVFS' (Branch _ header right) = do
    let file_ct = fileCount header
    files <- sequence [ getFile | x <- [1..file_ct] ]
    let subdir_ct = subdirCount header
    subdirs <- sequence [ getSubDir >>= getVFS' | x <- [1..subdir_ct] ]
    let things = files ++ subdirs
    let left = foldr nodeJoin Tip things
    return $ Branch left header right

getFileData :: Header -> Get BS.ByteString
getFileData (FileHeader _ size offset _) = do
    beforeFile <- getByteString $ fromEnum offset
    file <- getByteString $ fromEnum size
    return file