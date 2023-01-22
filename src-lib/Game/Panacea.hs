module Game.Panacea ( unpackVFS ) where

import Data.Binary.Get ( runGet )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Game.Panacea.Internal.Tree ( Tree(..) )
import Game.Panacea.VFS ( VFS, Header(..), name )
import Game.Panacea.VFS.Get ( getFileData )
import System.Directory ( createDirectoryIfMissing )

unpackVFS :: BSL.ByteString -> String -> VFS -> IO ()
unpackVFS _ _ Tip = return ()
unpackVFS vfsData cwd (Branch l (RootDirHeader _ _) _) = unpackVFS vfsData cwd l
unpackVFS vfsData cwd (Branch l x r) = do
    writeFileToDisk vfsData cwd x
    unpackVFS vfsData (cwd++"/"++name x) l
    unpackVFS vfsData cwd r

writeFileToDisk :: BSL.ByteString -> String -> Header -> IO ()
writeFileToDisk _ _ (RootDirHeader {}) = return ()
writeFileToDisk _ _ (SubDirHeader {}) = return ()
writeFileToDisk vfsData cwd fh@(FileHeader {}) = writeFileToDisk vfsData cwd $ runGet (getFileData fh) vfsData
writeFileToDisk _ cwd (FileHeaderFull fileName _ _ _ fileData) = createDirectoryIfMissing True cwd >> BS.writeFile (cwd++"/"++fileName) fileData