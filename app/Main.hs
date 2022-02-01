module Main where

import Prelude hiding (readFile)
import Control.Monad (when)
import Data.Binary (decode, encode)
import Data.Binary.Get (runGet)
import Data.ByteString.Lazy (readFile, hPut)
import Data.Maybe (fromMaybe)
import System.IO hiding (readFile)

import VFS

mainGetVFS = do
    myFile <- readFile "Scripts.vfs"
    fromMaybe (return ()) (sequence_ . (map $ putStrLn . show) . snd <$> (runGet getVFS myFile))
    

main = mainGetVFS