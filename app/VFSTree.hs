module VFSTree (
    VFS, getVFS, getNodeValue, flattenVFS
)
where

import Data.Binary (get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.UTF8 (toString)
import Data.Foldable (foldrM)
import Data.Word

-- put and get words with 8 bit length
putVFSStr :: String -> Put
putVFSStr str = do
    put (fromIntegral . length $ str :: Word8)
    sequence_ $ map put str

getVFSStr :: Get String
getVFSStr = do
    strlen <- getWord8
    str <- getByteString $ fromEnum strlen
    return . toString $ str

-- tree node for file tree, with Functor and Foldable instances
data Node a = Tip | Branch (Node a) a (Node a)

instance Functor Node where
    fmap _ Tip = Tip
    fmap f (Branch left a right) = Branch (fmap f left) (f a) (fmap f right)

instance Foldable Node where
    foldMap f Tip = mempty
    foldMap f (Branch left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

getNodeValue :: Node a -> a
getNodeValue (Branch _ a _) = a

flatten :: Node a -> [a]
flatten Tip = []
flatten root = foldr (:) [] root

-- individual header type
data Header =
    RootDirHeader !Word32 !Word32 |
    SubDirHeader  !String !Word32 !Word32 |
    FileHeader    !String !Word32 !Word32 !Word64
    deriving (Show)

-- determine if a given header is a file
isFile :: Header -> Bool
isFile (FileHeader _ _ _ _) = True
isFile _ = False

-- determine if a given header is a directory
isDir :: Header -> Bool
isDir = not . isFile

getName :: Header -> String
getName (RootDirHeader _ _) = "<root>"
getName (SubDirHeader name _ _) = name
getName (FileHeader name _ _ _) = name

getSubDirCount :: Header -> Word32
getSubDirCount (RootDirHeader sdct _) = sdct
getSubDirCount (SubDirHeader _ sdct _) = sdct

getFileCount :: Header -> Word32
getFileCount (RootDirHeader _ fct) = fct
getFileCount (SubDirHeader _ _ fct) = fct


type VFS = Node Header

getVFS :: Get VFS
getVFS = do
    root_dir <- getRootDir
    getDirFiles root_dir
    

getFile :: Get VFS
getFile = do
    name <- getVFSStr
    size <- getWord32le
    off  <- getWord32le
    time <- getWord64le
    return (Branch Tip (FileHeader name size off time) Tip)

vfsFoldFunc :: VFS -> VFS -> VFS
vfsFoldFunc (Branch left h1 _) h2 = Branch left h1 h2

getDirFiles :: VFS -> Get VFS
getDirFiles (Branch _ h _) = getDirFiles' . getFileCount $ h

getDirFiles' :: Word32 -> Get VFS
getDirFiles' fct = do
    files <- sequence [getFile | x <- [1..fct]] -- make strict?
    return $ foldr vfsFoldFunc Tip files

getRootDir :: Get VFS
getRootDir = do
    header <- getWord32le >>= getRootDir'
    return $ Branch Tip header Tip

getRootDir' :: Word32 -> Get Header
getRootDir' 0x4331504C = do
    subdir_ct <- getWord32le
    file_ct   <- getWord32le
    return (RootDirHeader subdir_ct file_ct)

getDir :: Get Header
getDir = do
    name <- getVFSStr
    subdir_ct <- getWord32le
    file_ct <- getWord32le
    return (SubDirHeader name subdir_ct file_ct)


--getSubDir :: VFS -> Get VFS
--getSubDir (Branch _ parent pr) = do
--    name      <- getVFSStr
--    subdir_ct <- getWord32le
--    file_ct   <- getWord32le
--    files <- sequence [getFile | x <- [1..file_ct]]
--    dirs <- foldrM (\_ vfs -> getSubDir vfs) Tip [1..subdir_ct]
--    let vfs = foldr vfsFoldFunc dirs files
--    return (Branch vfs parent pr)

fpred :: Header -> Bool
fpred (FileHeader _ _ _ _) = True
fpred _ = False

flattenVFS :: VFS -> [Header]
flattenVFS = (filter fpred) . flatten