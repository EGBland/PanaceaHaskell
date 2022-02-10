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

import qualified VFSTree as VT
import Tree

test_vfs = "Strings.vfs"

writeMaybe :: Maybe String -> Maybe BS.ByteString -> IO ()
writeMaybe (Just path) (Just x) = writeFile path x
writeMaybe _ _ = return ()

mainResolveFile :: String -> IO ()
mainResolveFile path = do
    theData <- readFile path
    let dirName = takeBaseName path
    putStrLn dirName
    let theVFS = runGet VT.getVFS theData
    let theFile = VT.getFileFromPath "UI/ui_bint.png" theVFS
    let theFileData = flip runGet theData <$> VT.getFileData <$> theFile
    writeMaybe (VT.getName <$> theFile) theFileData
    putStrLn . show $ theFile

mainExtract :: String -> IO ()
mainExtract path = do
    theData <- readFile path
    let dirName = takeBaseName path
    createDirectory dirName
    let theVFS = runGet VT.getVFS theData
    let (dct,fct) = VT.countFiles theVFS
    putStrLn $ printf "%d subdirectories, %d files" dct fct
    VT.extractVFS theData dirName theVFS

mainWriteTest :: String -> IO ()
mainWriteTest path = do
    theData <- readFile path
    let theVFS = runGet VT.getVFS theData
    let theHeader = runPut $ VT.putVFS theVFS
    writeFile "header.vfs" $ toStrict theHeader

main = mainWriteTest test_vfs