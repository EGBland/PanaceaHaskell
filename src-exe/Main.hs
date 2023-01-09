module Main where

import Data.Binary.Get ( runGet )
import qualified Data.ByteString.Lazy as BSL
import Game.Panacea.VFS.Get

main :: IO ()
main = BSL.readFile "Strings.vfs" >>= print . runGet getVFS