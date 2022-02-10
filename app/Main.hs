module Main where

import Prelude hiding (readFile, writeFile)

import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.ByteString (writeFile, toStrict)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (readFile)
import qualified Data.ByteString.Lazy as BSL
import System.Directory (createDirectory)
import System.FilePath (takeBaseName)
import System.IO hiding (readFile, writeFile)
import Text.Printf (printf)

import VFS
import VFS.Get
import VFS.Put
import Tree

test_vfs = "Textures.vfs"

main = do
    vfs <- readFile test_vfs >>= (\x -> return $ runGet getVFS x)
    let vfs2 = arrangeVFS vfs
    putStrLn . show . getNodeValue $ vfs
    putStrLn . show . getNodeValue $ vfs2
    writeFile "header.vfs" $ toStrict $ runPut $ putVFS vfs
    putStrLn "hello"