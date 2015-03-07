
module Main
(
  main
)
where

import qualified Test.UnicodeCMap
import qualified Test.Parser

import Test.Hspec

main :: IO ()
main = hspec $ do
  Test.UnicodeCMap.spec
  Test.Parser.spec
