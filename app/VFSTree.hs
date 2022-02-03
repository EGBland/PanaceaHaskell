module VFSTree (

)
where

import Data.Word
import Data.Binary (get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.UTF8 (toString)


putVFSStr :: String -> Put
putVFSStr str = do
    put (fromIntegral . length $ str :: Word8)
    sequence_ $ map put str

getVFSStr :: Get String
getVFSStr = do
    strlen <- getWord8
    str <- getByteString $ fromEnum strlen
    return . toString $ str


data Node a = Tip | Branch (Node a) a (Node a)

instance Functor Node where
    fmap _ Tip = Tip
    fmap f (Branch left a right) = Branch (fmap f left) (f a) (fmap f right)

instance Foldable Node where
    foldMap f Tip = mempty
    foldMap f (Branch left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

data Header =
    RootDirHeader !Word32 !Word32 |
    SubDirHeader  !String !Word32 !Word32 |
    FileHeader    !String !Word32 !Word32 !Word64

isFile :: Header -> Bool
isFile (FileHeader _ _ _ _) = True
isFile _ = False

isDir :: Header -> Bool
isDir = not . isFile


type VFS = Node Header

--getVFS :: Get (Maybe VFS)
--getVFS = do
--    root_dir <- getRootDir

getRootDir :: Get Header
getRootDir = do
    magic_number <- getWord32le
    getRootDir' magic_number

getRootDir' :: Word32 -> Get Header
getRootDir' 0x4331504C = do
    subdir_ct <- getWord32le
    file_ct   <- getWord32le
    return (RootDirHeader subdir_ct file_ct)

--getSubDir :: VFS -> Get VFS
--getSubDir parent = do
--    name      <- getVFSStr
--    subdir_ct <- getWord32le
--    file_ct   <- getWord32le
--    let files = [getFile :: x <- [1..file_ct]]
--    let vfs = foldr1 vfsFoldFunc 

getSubDir' :: Get (Header,VFS)
getSubDir' = do
    name      <- getVFSStr
    subdir_ct <- getWord32le
    file_ct   <- getWord32le

    files <- sequence [getFile | x <- [1..file_ct]]
    let vfs = foldr vfsFoldFunc Tip files
    return (SubDirHeader name subdir_ct file_ct, vfs)

vfsFoldFunc :: VFS -> VFS -> VFS
vfsFoldFunc (Branch left h1 _) h2 = Branch left h1 h2

getFile :: Get VFS
getFile = do
    name <- getVFSStr
    size <- getWord32le
    off  <- getWord32le
    time <- getWord64le
    return (Branch Tip (FileHeader name size off time) Tip)