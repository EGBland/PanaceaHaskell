module Game.Panacea ( unpackVFS,unpackVFS2 ) where

import Control.Monad ( guard, when )
import qualified Data.Bifunctor as BiF
import Data.Binary.Get ( runGet, getByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Game.Panacea.Internal.Tree ( Tree(..) )
import Game.Panacea.VFS ( VFS, Header(..), name )
import Game.Panacea.VFS.Get ( getFileData )
import Game.Panacea.VFS.Writer (PathWithRegion, asRegions)
import System.Directory ( createDirectoryIfMissing )
import Text.Printf ( printf )

unpackVFS :: BSL.ByteString -> String -> VFS -> IO ()
unpackVFS _ _ Tip = return ()
unpackVFS vfsData cwd (Branch l (RootDirHeader _ _) _) = unpackVFS vfsData cwd l
unpackVFS vfsData cwd (Branch l x r) = do
    writeFileToDisk vfsData cwd x
    unpackVFS vfsData (cwd++"/"++name x) l
    unpackVFS vfsData cwd r

unpackVFS2 :: BSL.ByteString -> String -> VFS -> IO ()
unpackVFS2 theData cwd vfs = do
    let theRegions = map (BiF.first (cwd++)) . asRegions $ vfs
    print theRegions
    unpack' theData theRegions 0

unpack' :: BSL.ByteString -> [PathWithRegion] -> Int -> IO ()
unpack' _ [] _ = return ()
unpack' theData ((p,(start,end)):xs) n = do
    putStrLn $ printf "[%s]\tCurrently at %d, getting (%d,%d), so dropping %d" p n start end (start-n)
    guard (start - n >= 0)
    let theData2 = BSL.drop (fromIntegral (start - n)) theData
    let theRelevantData = BSL.take (fromIntegral (end - start)) theData2
    let theNextData = BSL.drop (fromIntegral ((start - n) + (end - start))) theData
    myMkdir p >> BSL.writeFile p theRelevantData
    unpack' theNextData xs (n + (start - n) + (end - start))

myMkdir :: String -> IO ()
myMkdir = createDirectoryIfMissing True . reverse . dropWhile (/='/') . reverse


writeFileToDisk :: BSL.ByteString -> String -> Header -> IO ()
writeFileToDisk _ _ (RootDirHeader {}) = return ()
writeFileToDisk _ _ (SubDirHeader {}) = return ()
writeFileToDisk vfsData cwd fh@(FileHeader {}) = writeFileToDisk vfsData cwd $ runGet (getFileData fh) vfsData
writeFileToDisk _ cwd (FileHeaderFull fileName _ _ _ fileData) = createDirectoryIfMissing True cwd >> BS.writeFile (cwd++"/"++fileName) fileData