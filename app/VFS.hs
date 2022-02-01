module VFS (
    FileHeader, DirHeader, testHeader
)
where

import Data.Binary (Binary, Get, get, put)
import Data.Binary.Get (getByteString, getWord32le)
import Data.ByteString.UTF8 (toString)
import Data.Word

data FileHeader = FileHeader {
    filename  :: !String,
    filesize  :: !Word32,
    offset    :: !Word32,
    timestamp :: !Word64
    } deriving (Show)

data DirHeader = DirHeader {
    dirname   :: !String,
    subdir_ct :: !Word32,
    file_ct   :: !Word32
    } deriving (Show)

instance Binary DirHeader where
    put (DirHeader name sdct fct) = do
        --put (5 :: Word8)
        put name
        put sdct
        put fct
    
    get = do
        dirnamelen <- get :: Get Int
        name <- getByteString dirnamelen
        sdct <- getWord32le
        fct <- getWord32le
        return (DirHeader (toString name) sdct fct)

testHeader = DirHeader "Hello" 0 1