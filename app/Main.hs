module Main where

import Prelude hiding (readFile)
import Control.Monad (when)
import Data.Binary (decode, encode)
import Data.Binary.Get (runGet)
import Data.ByteString.Lazy (readFile, hPut)
import Data.Maybe (fromMaybe)
import System.IO hiding (readFile)

import VFS

printFiles :: ByteString -> IO ()
printFiles = (fromMaybe $ return ()) . (fmap $ sequence_ . (map $ putStrLn . show) . snd) . (runGet getVFS)

mainGetVFS = readFile "Scripts.vfs" >>= printFiles

main = mainGetVFS