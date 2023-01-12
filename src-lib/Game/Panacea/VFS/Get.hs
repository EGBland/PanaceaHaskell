{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Game.Panacea.VFS.Get ( getVFS, getFileData ) where

import Control.Monad ( guard, replicateM )
import Data.Binary.Get ( getWord32le, getWord64le, getWord8, getByteString, Get )
import Data.ByteString.UTF8 ( toString )
import Game.Panacea.VFS ( VFS, Header(..), subdirCount, fileCount )
import Game.Panacea.Internal.Tree ( Tree(Tip, Branch), fromList, appendTreeAsSibling, concatTree )

getString :: Get String
getString = fmap toString $ getWord8 >>= getByteString . fromEnum

getRootDir :: Get Header
getRootDir = do
    magic <- getWord32le
    guard (magic == 0x4331504C)
    numSubdirs <- getWord32le
    numFiles <- getWord32le
    return $ RootDirHeader numSubdirs numFiles

getSubdir :: Get Header
getSubdir = do
    name <- getString
    numSubdirs <- getWord32le
    numFiles <- getWord32le
    return $ SubDirHeader name numSubdirs numFiles

getFile :: Get Header
getFile = do
    name <- getString
    size <- getWord32le
    offset <- getWord32le
    lastModifiedTime <- getWord64le
    return $ FileHeader name size offset lastModifiedTime

getFileData :: Header -> Get Header
getFileData (FileHeader name size offset lastModifiedTime) = do
    _ <- getByteString . fromEnum $ offset
    fileData <- getByteString . fromEnum $ size
    return $ FileHeaderFull name size offset lastModifiedTime fileData
    

getVFS :: Get VFS
getVFS = getRootDir >>= getVFSDir

getVFSDir :: Header -> Get VFS
getVFSDir parentHeader = do
    subfiles <- replicateM (fromEnum . fileCount $ parentHeader) getFile
    subdirs  <- replicateM (fromEnum . subdirCount $ parentHeader) (getSubdir >>= getVFSDir)
    let
        subfileTree = fromList subfiles
        subdirTree = concatTree subdirs
        fullTree = appendTreeAsSibling subfileTree subdirTree
    return $ Branch fullTree parentHeader Tip