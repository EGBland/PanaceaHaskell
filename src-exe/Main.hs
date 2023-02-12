{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Data.Binary.Get ( runGet )
import Data.Binary.Put ( runPut )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as CSV
import qualified Data.Vector as V
import Game.Panacea ( unpackVFS )
import Game.Panacea.Strings ( StringRecord )
import Game.Panacea.Strings.Get ( getStrings )
import Game.Panacea.Strings.Put ( putStrings )
import Game.Panacea.VFS.Get ( getVFS )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.Environment ( getArgs, getProgName )
import Text.Printf ( printf )

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ("--unpack-vfs":xs) = parseVFSToUnpack xs
parseArgs ("--unpack-strings":xs) = parseStringFileToUnpack xs
parseArgs ("--pack-strings":xs) = parseStringCSVToPack xs
parseArgs _ = printUsage

parseVFSToUnpack :: [String] -> IO ()
parseVFSToUnpack [] = printUsage
parseVFSToUnpack (vfs:_) = doUnpackVFS vfs

parseStringFileToUnpack :: [String] -> IO ()
parseStringFileToUnpack (strFileName:_) = doUnpackStrings strFileName
parseStringFileToUnpack _ = printUsage

parseStringCSVToPack :: [String] -> IO ()
parseStringCSVToPack (csvName:_) = doPackStrings csvName
parseStringCSVToPack _ = printUsage

printUsage :: IO ()
printUsage = getProgName >>= printf "Usage: %s <option>\nWhere option is one of:\n--unpack-vfs <vfs name>      to unpack a VFS\n--unpack-strings <data file> to unpack a string data file\n--pack-strings <csv file>    to pack a string data file\n"

doUnpackVFS :: String -> IO ()
doUnpackVFS name = do
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

doUnpackStrings :: String -> IO ()
doUnpackStrings stringFileName = do
    sfData <- BSL.readFile stringFileName
    let strings = runGet getStrings sfData
    let theCsv = CSV.encode strings
    BSL.writeFile "text.csv" theCsv

doPackStrings :: String -> IO ()
doPackStrings csvName = do
    csvData <- BSL.readFile csvName
    let records = V.toList <$> CSV.decode CSV.NoHeader csvData :: Either String [StringRecord]
    case records of
        Left theError -> putStrLn "A CSV error occurred:" >> putStrLn theError
        Right theRecords -> packStrings theRecords

packStrings :: [StringRecord] -> IO ()
packStrings records = do
    let packed = runPut $ putStrings records
    BSL.writeFile "main_modified.dat" packed