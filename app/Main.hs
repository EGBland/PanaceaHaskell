module Main where

import Prelude hiding (readFile)
import Control.Monad (when)
import Data.Binary (decode, encode)
import Data.Binary.Get (runGet)
import Data.ByteString.Lazy (ByteString, readFile, hPut)
import Data.Maybe (fromMaybe)
import System.IO hiding (readFile)

import qualified VFS as V
import qualified VFSTree as VT


printFilesV :: ByteString -> IO ()
printFilesV = (fromMaybe $ return ()) . (fmap $ sequence_ . (map $ putStrLn . show) . snd) . (runGet V.getVFS)

--printFilesVT :: ByteString -> VT.VFS
printFilesVT = sequence_ . (map $ putStrLn . show) . VT.flattenVFS . (runGet VT.getVFS)

test_vfs = "Textures.vfs"
mainGetV = readFile test_vfs >>= printFilesV
mainGetVT = readFile test_vfs >>= printFilesVT

mainGetFile :: IO ()
mainGetFile = do
    theData <- readFile test_vfs
    let theVFS = runGet VT.getVFS theData
    let files = VT.flattenVFS theVFS
    putStrLn "hello"

main = mainGetVT