module VFSTree (
    VFS, getVFS, getNodeValue, flattenVFS,
    Header, namePred, getFileData, getFileFromPath,
    getName, split
)
where

import Data.Binary (get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust, isNothing)
import Data.Word

import Tree

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split c s
    | head s == c = split c $ tail s
    | otherwise = [sBefore]++(split c sNext)
    where sBefore = takeWhile (/=c) s
          sNext = drop (length sBefore) s

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


------------------- DIR STUFF -------------------
type VFS = Node Header

getRootDirHeader :: Get Header
getRootDirHeader = getWord32le >>= getRootDirHeader'

getRootDirHeader' :: Word32 -> Get Header
getRootDirHeader' 0x4331504C = do
    subdir_ct <- getWord32le
    file_ct   <- getWord32le
    return (RootDirHeader subdir_ct file_ct)

getSubDirHeader :: Get Header
getSubDirHeader = do
    name <- getVFSStr
    subdir_ct <- getWord32le
    file_ct <- getWord32le
    return (SubDirHeader name subdir_ct file_ct)

getRootDir :: Get VFS
getRootDir = do
    header <- getRootDirHeader
    return (Branch Tip header Tip)

getSubDir :: Get VFS
getSubDir = do
    header <- getSubDirHeader
    return (Branch Tip header Tip)

getFile :: Get VFS
getFile = do
    name <- getVFSStr
    size <- getWord32le
    off  <- getWord32le
    time <- getWord64le
    return (Branch Tip (FileHeader name size off time) Tip)

getVFS :: Get VFS
getVFS = getRootDir >>= getVFS'

getVFS' :: VFS -> Get VFS
getVFS' (Branch _ header right) = do
    let file_ct = getFileCount header
    files <- sequence [ getFile | x <- [1..file_ct] ]
    let subdir_ct = getSubDirCount header
    subdirs <- sequence [ getSubDir >>= getVFS' | x <- [1..subdir_ct] ]
    let things = files ++ subdirs
    let left = foldr nodeJoin Tip things
    return $ Branch left header right 


fpred :: Header -> Bool
fpred (FileHeader _ _ _ _) = True
fpred _ = False

flattenVFS :: VFS -> [Header]
flattenVFS = flatten

pathResolver :: String -> Header -> Bool
pathResolver name = (==name) . getName

getFileFromPath :: String -> VFS -> Get ByteString
getFileFromPath path (Branch left (RootDirHeader _ _) _) = getFileFromPath path left
getFileFromPath path root
    | isNothing resolved = error "oh bugger"
    | otherwise = getFileData . getNodeValue . fromJust $ resolved
    where pathSegments = split '/' path
          resolved     = resolveBy pathResolver pathSegments root

getFileData :: Header -> Get ByteString
getFileData (FileHeader _ size offset _) = do
    beforeFile <- getByteString $ fromEnum offset
    file <- getByteString $ fromEnum size
    return file

namePred :: String -> Header -> Bool
namePred cand (FileHeader name _ _ _) = name == cand
namePred cand (SubDirHeader name _ _) = name == cand
namePred _ _ = False