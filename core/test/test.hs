
module Main
(
  main
)
where

import qualified Test.XRef
import qualified Test.Stream
import qualified Test.Parsers.Object
import qualified Test.Object.Builder
import qualified Test.Object.Util
import qualified Test.Name

import Test.Hspec

main :: IO ()
main = hspec $ do
  Test.XRef.spec
  Test.Stream.spec
  Test.Parsers.Object.spec
  Test.Object.Builder.spec
  Test.Object.Util.spec
  Test.Name.spec
