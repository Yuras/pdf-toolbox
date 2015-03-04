
module Main
(
  main
)
where

import qualified Test.UnicodeCMap

import Test.Hspec

main :: IO ()
main = hspec $ do
  Test.UnicodeCMap.spec
