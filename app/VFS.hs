module VFS (
    FileHeader, RootDirHeader, SubDirHeader, getVFS
)
where

import Data.Binary (Binary, Get, Put, get, put)
import Data.Binary.Get (getByteString, getWord8, getWord32le, getWord64le)
import Data.Binary.Put (putWord32le, putWord64le)
import Data.ByteString.UTF8 (toString)
import Data.Word
import Text.Printf (printf)

-- put & get functions for strings that use Word8 instead of Word64be for length
putVFSStr :: String -> Put
putVFSStr str = do
    put (fromIntegral . length $ str :: Word8)
    sequence_ $ map put str

getVFSStr :: Get String
getVFSStr = do
    strlen <- getWord8
    str <- getByteString $ fromEnum strlen
    return . toString $ str


-- Datatypes for headers present in VFS files
data FileHeader = FileHeader {
    filename  :: !String,
    filesize  :: !Word32,
    offset    :: !Word32,
    timestamp :: !Word64
    }

instance Show FileHeader where
    show (FileHeader name size offset timestamp) =
        let
            sizeKb = fromIntegral size / 1024 :: Float
        in
            printf "%s\t%.2fKB\t%d\t%d" name sizeKb offset timestamp

data RootDirHeader = RootDirHeader {
        rd_subdir_ct :: !Word32,
        rd_file_ct   :: !Word32
    } deriving (Show)
data SubDirHeader  = SubDirHeader {
        sd_name      :: !String,
        sd_subdir_ct :: !Word32,
        sd_file_ct   :: !Word32
    } deriving (Show)


-- Binary instances for VFS headers
instance Binary FileHeader where
    put (FileHeader name size off time) = do
        putVFSStr   name
        putWord32le size
        putWord32le off
        putWord64le time
    
    get = do
        name <- getVFSStr
        size <- getWord32le
        off  <- getWord32le
        time <- getWord64le
        return (FileHeader name size off time)

instance Binary SubDirHeader where
    put (SubDirHeader name subdir_ct file_ct) = do
        putVFSStr    name
        putWord32le subdir_ct
        putWord32le file_ct
    
    get = do
        name      <- getVFSStr
        subdir_ct <- getWord32le
        file_ct   <- getWord32le
        return (SubDirHeader name subdir_ct file_ct)

instance Binary RootDirHeader where
    put (RootDirHeader subdir_ct file_ct) = do
        putWord32le 0x4331504C -- magic number
        putWord32le subdir_ct
        putWord32le file_ct

    get = do
        magic_number <- getWord32le
        checkMagicNumber magic_number

checkMagicNumber :: Word32 -> Get RootDirHeader
checkMagicNumber 0x4331504C = do
    subdir_ct <- getWord32le
    file_ct   <- getWord32le
    return $ RootDirHeader subdir_ct file_ct
checkMagicNumber _ = error "wrong magic number"


-- the VFS type
type VFS = (RootDirHeader,[FileHeader])
getVFS :: Get (Maybe VFS)
getVFS = do
    magic_number <- getWord32le :: Get Word32
    getVFS' magic_number

getVFS' :: Word32 -> Get (Maybe VFS)
getVFS' 0x4331504C = do
    subdir_ct <- getWord32le
    file_ct   <- getWord32le
    let fileGets = foldr (\_ acc -> (get :: Get FileHeader):acc) [] [1..file_ct]
    
    files <- sequence fileGets
    return $ Just (RootDirHeader subdir_ct file_ct,files)
getVFS' _ = return Nothing

getSubDir :: Get (SubDirHeader,[FileHeader])
getSubDir = do
    subdir_header <- get :: Get SubDirHeader
    files <- getFiles $ sd_file_ct subdir_header
    return (subdir_header, files)

getFiles :: Word32 -> Get [FileHeader]
getFiles n = sequence [get :: Get FileHeader | x <- [1..n]]

--getFiles :: Word32 -> Get [FileHeader]