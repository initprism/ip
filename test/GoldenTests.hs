module GoldenTests where

import Test.Tasty
import Test.Tasty.Golden

import System.FilePath (normalise, takeBaseName, replaceExtension)

import Lookup.ParseIP
import Lookup.LookupIP

goldenTests :: IO [TestTree]
goldenTests = sequence [goldenLookupIP]


testDir :: FilePath
testDir = normalise "data/tests/"

goldenLookupIP :: IO TestTree
goldenLookupIP = testGroup "lookupIP" . map createTest
                    <$> findByExtension [".iprs"] testDir


createTest :: String -> TestTree
createTest iprsf = goldenVsFile (takeBaseName iprsf) goldenf outf testAction  
    where
        ipsf    = replaceExtension iprsf ".ips"
        goldenf = replaceExtension iprsf ".out.golden"
        outf    = replaceExtension iprsf ".out"
        testAction = do
            iprs <- parseValidIPRanges <$> readFile iprsf
            ips  <- parseValidIPs <$> readFile ipsf
            writeBinaryFile outf $ reportIPs iprs ips

