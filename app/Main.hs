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

mainGetV = readFile "Scenes.vfs" >>= printFilesV
mainGetVT = readFile "Scenes.vfs" >>= printFilesVT

main = mainGetVT