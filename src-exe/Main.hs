module Main where

import Data.Binary.Get ( Get, runGet )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Game.Panacea.VFS.Get
import Game.Panacea.VFS ( resolve, fileData )

runMaybeGet :: Maybe (Get a) -> BSL.ByteString -> Maybe a
runMaybeGet (Just g) bs = Just $ runGet g bs
runMaybeGet Nothing _ = Nothing

main :: IO ()
main = do
    vfsData <- BSL.readFile "Textures.vfs"
    let vfs = runGet getVFS vfsData
    print vfs
    let resolveResult = resolve "UI/NPC_Rubin_b.png" vfs
    let theGet = getFileData <$> resolveResult
    let theResult = runMaybeGet theGet vfsData
    case theResult of
        Just x -> BS.writeFile "result.png" . fileData $ x
        Nothing -> return ()