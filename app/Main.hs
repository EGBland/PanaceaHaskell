module Main where

import Prelude hiding (readFile, writeFile)

import Data.Binary.Get (runGet)
import Data.ByteString (writeFile)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (readFile)
import qualified Data.ByteString.Lazy as BSL
import System.Directory (createDirectory)
import System.FilePath (takeBaseName)
import System.IO hiding (readFile, writeFile)

import qualified VFSTree as VT
import Tree

test_vfs = "Textures.vfs"

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
    VT.extractVFS theData dirName theVFS

main = mainExtract test_vfs