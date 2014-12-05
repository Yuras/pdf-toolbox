{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec
import           Test.HUnit
import           System.IO

import           Pdf.Toolbox.Document

main :: IO ()
main = hspec suite

testFile1 :: FilePath
testFile1 = "./tests/data/test1.pdf"

testFile2 :: FilePath
testFile2 = "./tests/data/test2.pdf"

suite :: Spec
suite = describe "runPdfWithHandle" $ do
    it "successfully loads a valid PDF" $ do
        r <- withBinaryFile testFile1 ReadMode $ \h ->
                runPdfWithHandle h knownFilters $ do
                    document >>= documentCatalog
        case r of
            (Right _) -> return ()
            (Left m) -> assertFailure (show m)

    it "does not successfully load an invalid PDF" $ do
        r <- withBinaryFile testFile2 ReadMode $ \h ->
                runPdfWithHandle h knownFilters $ do
                    document >>= documentCatalog
        case r of
            (Right m) -> assertFailure $ "Successfully loaded invalid document with catalog " ++ (show m)
            (Left _) -> return ()
