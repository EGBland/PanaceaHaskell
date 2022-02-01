module Main where

import Prelude hiding (readFile)
import Data.Binary (decode, encode)
import Data.Binary.Get (runGet)
import Data.ByteString.Lazy (readFile, hPut)
import System.IO hiding (readFile)

import VFS

mainPut :: IO ()
mainPut = do
    myFile <- openFile "Test.vfs" WriteMode
    putStrLn . show $ testHeader
    hPut myFile (encode testHeader)
    hClose myFile

mainGet :: IO ()
mainGet = do
    myFile <- readFile "Test.vfs"
    putStrLn . show $ (decode myFile :: DirHeader)

mainGetVFS :: IO ()
mainGetVFS = do
    myFile <- readFile "Scenes.vfs"
    let myVFS = runGet getVFS myFile
    putStrLn . show $ myVFS

main :: IO ()
main = mainGetVFS