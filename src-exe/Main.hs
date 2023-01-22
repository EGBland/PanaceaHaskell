module Main where

import Data.Binary.Get ( runGet )
import qualified Data.ByteString.Lazy as BSL
import Game.Panacea ( unpackVFS )
import Game.Panacea.VFS.Get ( getVFS )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.Environment ( getArgs )
import Text.Printf ( printf )

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn "Usage: panacea <VFS name to unpack>"
parseArgs (x:_) = unpack x

unpack :: String -> IO ()
unpack name = do
    let vfsFileName = printf "%s.vfs" name :: String
    vfsExists <- doesFileExist vfsFileName
    if
        vfsExists
    then do
        vfsData <- BSL.readFile vfsFileName
        let vfs = runGet getVFS vfsData
        print vfs
        createDirectoryIfMissing True name
        unpackVFS vfsData name vfs
    else
        putStrLn $ printf "File \"%s\" does not exist. Check your path is correct, and ensure you do not affix \".vfs\" to the entered file name." vfsFileName