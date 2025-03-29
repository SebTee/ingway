module Main (main) where

import Test.Hspec
import ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  parserSpec
