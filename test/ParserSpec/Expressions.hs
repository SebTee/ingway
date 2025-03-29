module ParserSpec.Expressions where

import Test.Hspec
import Text.Ingway.Parser
import ParserSpec.HelperFunctions

expressionsSpec :: Spec
expressionsSpec = describe "Expressions" $ do
  describe "ident" $ do
    let s = parses ident
    let f = failsToParse ident
    context "when given a single character ident" $ do
      it "parses the ident" $ do
        s "x" $ Ident "x"
    context "when given an invalid single character ident" $ do
      it "fails to parse a number" $ do
        f "3"
      it "fails to parse an underscore" $ do
        f "_"
      it "fails to parse an at symbol" $ do
        f "@"
    context "when given a multiple character ident" $ do
      it "parses a 2 character ident" $ do
        s "Ab" $ Ident "Ab"
      it "parses a 3 character ident" $ do
        s "AbC" $ Ident "AbC"
      it "parses a ident with numbers" $ do
        s "abc123" $ Ident "abc123"
      it "parses a ident with underscore" $ do
        s "abc_" $ Ident "abc_"
    context "when given an invalid multiple character ident" $ do
      it "fails to parse an ident that starts with a number" $ do
        f "1abc"
      it "fails to parse an ident that starts with an underscore" $ do
        f "_abc"
      it "fails to parse an ident that starts with an at symbol" $ do
        f "@abc"
  describe "ident" $ do
    let s = parses expression
    let f = failsToParse expression
    context "when given a literal" $ do
      it "parses a number" $ do
        s "12" $ Lit $ NumLit 12
      it "parses a character" $ do
        s "'c'" $ Lit $ CharLit 'c'
      it "parses a string" $ do
        s "\"Hello, World\"" $ Lit $ StrLit "Hello, World"
    context "when given an ident" $ do
      it "parses the indent" $ do
        s "x" $ Var $ Ident "x"
    context "when given a parenthesized expression" $ do
      it "parses the parenthesized expression" $ do
        s "(x)" $ Var $ Ident "x"
      it "parses the parenthesized expression with spaces" $ do
        s "( x )" $ Var $ Ident "x"
      it "parses the nested parenthesized expression" $ do
        s "((x))" $ Var $ Ident "x"
    context "when given a non closed parenthesized expression" $ do
      it "failes to parse" $ do
        f "(x"
    context "when given a function application" $ do
      it "parses a unary function application" $ do
        s "succ 1" $ App (Var $ Ident "succ") (Lit $ NumLit 1)
      it "parses a binary function application" $ do
        s "add 1 2" $ App (App (Var $ Ident "add") (Lit $ NumLit 1)) (Lit $ NumLit 2)
      it "parses a parenthesized expression being passes to a function" $ do
        s "succ (succ 0)" $ App (Var $ Ident "succ") (App (Var $ Ident "succ") (Lit $ NumLit 0))