{-# LANGUAGE FlexibleContexts #-}
module ParserSpec.HelperFunctions where

import Test.Hspec
import Text.Parsec
import Data.Functor.Identity (Identity)
import Text.Ingway.Parser
import Data.List (intercalate)
import Text.Parsec.Error

parsesChar :: String -> Char -> Expectation
parsesChar input expected = charLit `parses` input $ CharLit expected

parsesString :: String -> String -> Expectation
parsesString input expected = strLit `parses` input $ StrLit expected

parses :: (Stream s Identity t, Eq a, Show a) => Parsec s () a -> s -> a -> Expectation
parses parser input expected = case parse parser "" input of
  Right result -> result `shouldBe` expected
  Left e -> expectationFailure $ "Parse failed with errors:\n" ++
    intercalate "\n" (messageString <$> errorMessages e)

failsToParse :: (Stream s Identity t, Eq a, Show a) => Parsec s () a -> s -> Expectation
failsToParse parser input = case parse parser "" input of
  Right result -> expectationFailure $ "Expected parse to fail, but got: " ++ show result
  Left _ -> return ()