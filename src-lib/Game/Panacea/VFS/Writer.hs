{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Game.Panacea.VFS.Writer (PathWithRegion, asRegions) where

import Data.List ( sortOn )
import Game.Panacea.VFS

type PathWithRegion = (String,(Int,Int))

asRegions :: VFS -> [PathWithRegion]
asRegions = sortOn (fst . snd) . snd . foldl (flip asRegionsFoldr) ("",[])

asRegionsFoldr :: Header -> (String,[PathWithRegion]) -> (String,[PathWithRegion])
asRegionsFoldr (RootDirHeader _ _) _ = ("",[])
asRegionsFoldr header (cwd,acc) =
    if
        isFile header
    then
        (cwd,(thePath,theRegion):acc)
    else
        (thePath,acc)
    where
        theName = name header
        thePath = cwd ++ "/" ++ theName
        theRegion = getRegionInterval header

getRegionInterval :: Header -> (Int,Int)
getRegionInterval (FileHeader _ size offset _) = (fromEnum offset, fromEnum (size+offset))
getRegionInterval (FileHeaderFull _ size offset _ _) = (fromEnum offset, fromEnum (size+offset))