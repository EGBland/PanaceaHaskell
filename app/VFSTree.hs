module VFSTree (
    VFS
)
where

import Data.Binary (get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.UTF8 (toString)
import Data.Foldable (foldrM)
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

--getVFS :: Get VFS
--getVFS = do
    

getRootDir :: Get VFS
getRootDir = do
    header <- (getWord32le >>= getRootDir')
    return Tip

getRootDir' :: Word32 -> Get Header
getRootDir' 0x4331504C = do
    subdir_ct <- getWord32le
    file_ct   <- getWord32le
    return (RootDirHeader subdir_ct file_ct)

getSubDir :: VFS -> Get VFS
getSubDir (Branch _ parent pr) = do
    name      <- getVFSStr
    subdir_ct <- getWord32le
    file_ct   <- getWord32le
    files <- sequence [getFile | x <- [1..file_ct]]
    dirs <- foldrM (\_ vfs -> getSubDir vfs) Tip [1..subdir_ct]
    let vfs = foldr vfsFoldFunc dirs files
    return (Branch vfs parent pr)

vfsFoldFunc :: VFS -> VFS -> VFS
vfsFoldFunc (Branch left h1 _) h2 = Branch left h1 h2

getFile :: Get VFS
getFile = do
    name <- getVFSStr
    size <- getWord32le
    off  <- getWord32le
    time <- getWord64le
    return (Branch Tip (FileHeader name size off time) Tip)