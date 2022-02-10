module VFS.Put (
    putVFS
)
where

import Data.Binary (put)
import Data.Binary.Put
import Data.Word

import VFS.Prelude

putVFSStr :: String -> Put
putVFSStr str = do
    put (fromIntegral . length $ str :: Word8)
    sequence_ $ map put str

foldPut :: Header -> Put -> Put
foldPut (RootDirHeader sdct fct) acc = do
    putWord32le 0x4331504C
    putWord32le sdct
    putWord32le fct
    acc

foldPut (SubDirHeader name sdct fct) acc = do
    putVFSStr name
    putWord32le sdct
    putWord32le fct
    acc

foldPut (FileHeader name size offset time) acc = do
    putVFSStr name
    putWord32le size
    putWord32le offset
    putWord64le time
    acc

putVFS :: VFS -> Put
putVFS = foldr foldPut $ return ()