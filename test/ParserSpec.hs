{-# LANGUAGE FlexibleContexts #-}
module ParserSpec (parserSpec) where

import Test.Hspec
import ParserSpec.Literals

parserSpec :: Spec
parserSpec = describe "Parser" $ do
  literalsSpec

