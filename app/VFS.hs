module VFS (
    FileHeader, DirHeader, testHeader, getVFS
)
where

import Data.Binary (Binary, Get, Put, get, put)
import Data.Binary.Get (getByteString, getWord8, getWord32le, getWord64le)
import Data.Binary.Put (putWord32le, putWord64le)
import Data.ByteString.UTF8 (toString)
import Data.Word

putVFSStr :: String -> Put
putVFSStr str = do
    put (fromIntegral . length $ str :: Word8)
    sequence_ $ map put str

getVFSStr :: Get String
getVFSStr = do
    strlen <- getWord8
    str <- getByteString $ fromEnum strlen
    return . toString $ str

data FileHeader = FileHeader {
    filename  :: !String,
    filesize  :: !Word32,
    offset    :: !Word32,
    timestamp :: !Word64
    } deriving (Show)

instance Binary FileHeader where
    put (FileHeader name size off time) = do
        putVFSStr name
        putWord32le size
        putWord32le off
        putWord64le time
    
    get = do
        name <- getVFSStr
        size <- getWord32le
        off  <- getWord32le
        time <- getWord64le
        return (FileHeader name size off time)

data DirHeader = RootDirHeader Word32 Word32 | SubDirHeader {
    dirname   :: !String,
    subdir_ct :: !Word32,
    file_ct   :: !Word32
    } deriving (Show)

instance Binary DirHeader where
    put (RootDirHeader sdct fct) = do
        putWord32le 0x4331504C
        putWord32le sdct
        putWord32le fct
    put (SubDirHeader name sdct fct) = do
        putVFSStr name
        putWord32le sdct
        putWord32le fct
    
    get = do
        name <- getVFSStr
        sdct <- getWord32le
        fct <- getWord32le
        return (SubDirHeader name sdct fct)

type VFS = [(DirHeader,[FileHeader])]
getVFS :: Get (Either String VFS)
getVFS = do
    magic_number <- getWord32le :: Get Word32
    getVFS' magic_number

getVFS' :: Word32 -> Get (Either String VFS)
getVFS' 0x4331504C = do
    dir_count  <- getWord32le
    file_count <- getWord32le
    let fileGets = foldr (\_ acc -> (get :: Get FileHeader):acc) [] [1..file_count]
    files <- sequence fileGets
    return $ Right [(testHeader, files)]
getVFS' _ = return $ Left "wrong magic number"

--getFiles :: Word32 -> Get [FileHeader]


testHeader = SubDirHeader "Hello" 0 1