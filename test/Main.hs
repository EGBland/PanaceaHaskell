module Main (main) where

import Control.Exception as E
import Data.Binary.Get ( runGet )
import qualified Data.ByteString.Lazy as BSL
import Game.Panacea.Internal.Tree ( left )
import Game.Panacea.VFS
import Game.Panacea.VFS.Get
import Test.HUnit

assertIsRootDirHeader :: Header -> Assertion
assertIsRootDirHeader (RootDirHeader _ _) = return ()
assertIsRootDirHeader header = assertFailure $ "Expected a RootDirHeader, but got: " ++ show header

testTreeRootIsRootDirHeader :: VFS -> Test
testTreeRootIsRootDirHeader vfs = TestCase $ case value vfs of
    Nothing         -> assertFailure $ "Expected a non-empty VFS, but got an empty VFS"
    Just rootHeader -> assertIsRootDirHeader rootHeader

testingVFS :: IO VFS
testingVFS = left . runGet getVFS <$> BSL.readFile "Strings.vfs"
--testVFS = return empty

main :: IO ()
main = do
    vfs <- testingVFS
    let tests = TestList [TestLabel "VFS root is RootDirHeader" $ testTreeRootIsRootDirHeader vfs]
    runTestTT tests >>= print
