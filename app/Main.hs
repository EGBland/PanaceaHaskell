module Main where

import Prelude hiding (readFile)
import Data.Binary (decode, encode)
import Data.ByteString.Lazy (readFile, hPut)
import System.IO hiding (readFile)

import VFS

mainPut = do
    myFile <- openFile "Test.vfs" WriteMode
    putStrLn . show $ testHeader
    hPut myFile (encode testHeader)
    hClose myFile

mainGet :: IO ()
mainGet = do
    myFile <- readFile "Test.vfs"
    putStrLn . show $ (decode myFile :: DirHeader)

main = mainPut