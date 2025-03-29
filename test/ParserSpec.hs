{-# LANGUAGE FlexibleContexts #-}
module ParserSpec (parserSpec) where

import Test.Hspec
import ParserSpec.Literals
import ParserSpec.Expressions

parserSpec :: Spec
parserSpec = describe "Parser" $ do
  literalsSpec
  expressionsSpec
