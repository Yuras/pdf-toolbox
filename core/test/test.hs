
module Main
(
  main
)
where

import qualified Test.XRef
import qualified Test.Stream

import Test.Hspec

main :: IO ()
main = hspec $ do
  Test.XRef.spec
  Test.Stream.spec
