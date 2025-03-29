module ParserSpec.Literals (literalsSpec) where

import Test.Hspec
import Text.Ingway.Parser
import ParserSpec.HelperFunctions

literalsSpec :: Spec
literalsSpec = describe "Literals" $ do
  describe "numberLit" $ do
    let s = parses numberLit       -- succeeds parse
    let f = failsToParse numberLit -- fails to parse
    context "when given a positive integer" $ do
      it "parses the number" $ do
        s "123" 123
    context "when given a negative integer" $ do
      it "parses the number" $ do
        s "-123" (-123)
    context "when given a positive fraction" $ do
      it "parses the number" $ do
        s "12.3" 12.3
    context "when given a negative fraction" $ do
      it "parses the number" $ do
        s "-12.3" (-12.3)
    context "when given a positive number with exponent" $ do
      it "parses the number" $ do
        s "12.3e3" 12300
    context "when given a negative number with exponent" $ do
      it "parses the number" $ do
        s "-12.3e3" (-12300)
    context "when given a positive number with exponent with capital E" $ do
      it "parses the number" $ do
        s "12.3E3" 12300
    context "when given a negative number with exponent with capital E" $ do
      it "parses the number" $ do
        s "-12.3E3" (-12300)
    context "when given a positive number with exponent" $ do
      it "parses the number" $ do
        s "123e3" 123000
    context "when given a negative number with exponent" $ do
      it "parses the number" $ do
        s "-123e3" (-123000)
    context "when given a positive number with negative exponent" $ do
      it "parses the number" $ do
        s "123e-3" 0.123
    context "when given a negative number with negative exponent" $ do
      it "parses the number" $ do
        s "-123e-3" (-0.123)
    context "when given a positive fraction with negative exponent" $ do
      it "parses the number" $ do
        s "12.3e-3" 0.0123
    context "when given a negative number with negative exponent" $ do
      it "parses the number" $ do
        s "-12.3e-3" (-0.0123)
    context "when given a decimal point with no integer part" $ do
      it "fails to parse" $ do
        f ".123"
    context "when given a decimal point with no fraction part" $ do
      it "fails to parse" $ do
        f "123."
    context "when given a e with no exponent" $ do
      it "fails to parse" $ do
        f "123e"

  describe "charLit" $ do
    let s = parses charLit       -- succeeds parse
    let f = failsToParse charLit -- fails to parse
    context "when given a character literal" $ do
      it "parses the character" $ do
        s "'a'" 'a'
    context "when given a valid escaped character" $ do
      it "parses new line" $ do
        s "'\\n'" '\n'
      it "parses carriage return" $ do
        s "'\\r'" '\r'
      it "parses form feed" $ do
        s "'\\f'" '\f'
      it "parses backspace" $ do
        s "'\\b'" '\b'
      it "parses tab" $ do
        s "'\\t'" '\t'
      it "parses single quote" $ do
        s "'\\''" '\''
      it "parses double quote" $ do
        s "'\\\"'" '"'
      it "parses backslash" $ do
        s "'\\\\'" '\\'
      it "parses null" $ do
        s "'\0'" '\NUL'
    context "when given an invalid escape character" $ do
      it "fails to parse" $ do
        f "'\\z'"
    context "when given an valid 4 digit hex character" $ do
      it "parses the hex character" $ do
        mapM_ (uncurry s) [ ("'\\x0061'", 'a')
                                   , ("'\\x0041'", 'A')
                                   , ("'\\x0030'", '0')
                                   , ("'\\x0039'", '9')
                                   , ("'\\x0046'", 'F')
                                   , ("'\\xaBcD'", '\xabcd')
                                   ]
    context "when given an invalid 4 digit hex character" $ do
      it "fails to parse" $ do
        f "'\\x00z0'"
    context "when given an invalid length hex character" $ do
      it "fails to parse" $ do
        mapM_ f [ "'\\x0'"
                , "'\\x'"
                , "'\\x000'"
                , "'\\x00000'"
                ]
    context "when given an empty character literal" $ do
      it "fails to parse" $ do
        f "''"
    context "when given a character literal with no closing quote" $ do
      it "fails to parse" $ do
        f "'a"

  describe "strLit" $ do
    let s = parses strLit       -- succeeds parse
    let f = failsToParse strLit -- fails to parse
    context "when given a string literal" $ do
      it "parses the string" $ do
        s "\"hello\"" "hello"
    context "when given a string with escaped characters" $ do
      it "parses the string" $ do
        s "\"\\n\\r\\f\\b\\t\\'\\\"\\\\\\0\"" "\n\r\f\b\t'\"\\\0"
    context "when given a string with escaped hex characters" $ do
      it "parses the string" $ do
        s "\"\\x0061\\x0041\\x0030\\x0039\\x0046\\xabcd\"" "aA09F\xabcd"
    context "when given an empty string literal" $ do
      it "parses the empty string" $ do
        s "\"\"" ""
    context "when given a string with invalid escaped characters" $ do
      it "fails to parse" $ do
        f "\"\\z\""
    context "when given a string with invalid escaped hex characters" $ do
      it "fails to parse" $ do
        f "\"\\x00z0\""
    context "when given a string with no closing quote" $ do
      it "fails to parse" $ do
        f "\"hello"