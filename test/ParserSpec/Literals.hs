module ParserSpec.Literals (literalsSpec) where

import Test.Hspec
import Text.Ingway.Parser
import ParserSpec.HelperFunctions

literalsSpec :: Spec
literalsSpec = describe "Literals" $ do
  describe "numberLit" $ do
    context "when given a positive integer" $ do
      it "parses the number" $ do
        parsesNumber "123" 123
    context "when given a negative integer" $ do
      it "parses the number" $ do
        parsesNumber "-123" (-123)
    context "when given a positive fraction" $ do
      it "parses the number" $ do
        parsesNumber "12.3" 12.3
    context "when given a negative fraction" $ do
      it "parses the number" $ do
        parsesNumber "-12.3" (-12.3)
    context "when given a positive number with exponent" $ do
      it "parses the number" $ do
        parsesNumber "12.3e3" 12300
    context "when given a negative number with exponent" $ do
      it "parses the number" $ do
        parsesNumber "-12.3e3" (-12300)
    context "when given a positive number with exponent with capital E" $ do
      it "parses the number" $ do
        parsesNumber "12.3E3" 12300
    context "when given a negative number with exponent with capital E" $ do
      it "parses the number" $ do
        parsesNumber "-12.3E3" (-12300)
    context "when given a positive number with exponent" $ do
      it "parses the number" $ do
        parsesNumber "123e3" 123000
    context "when given a negative number with exponent" $ do
      it "parses the number" $ do
        parsesNumber "-123e3" (-123000)
    context "when given a positive number with negative exponent" $ do
      it "parses the number" $ do
        parsesNumber "123e-3" 0.123
    context "when given a negative number with negative exponent" $ do
      it "parses the number" $ do
        parsesNumber "-123e-3" (-0.123)
    context "when given a positive fraction with negative exponent" $ do
      it "parses the number" $ do
        parsesNumber "12.3e-3" 0.0123
    context "when given a negative number with negative exponent" $ do
      it "parses the number" $ do
        parsesNumber "-12.3e-3" (-0.0123)
    context "when given a decimal point with no integer part" $ do
      it "fails to parse" $ do
        failsToParse numberLit ".123"
    context "when given a decimal point with no fraction part" $ do
      it "fails to parse" $ do
        failsToParse numberLit "123."
    context "when given a e with no exponent" $ do
      it "fails to parse" $ do
        failsToParse numberLit "123e"

  describe "charLit" $ do
    context "when given a character literal" $ do
      it "parses the character" $ do
        parsesChar "'a'" 'a'
    context "when given a valid escaped character" $ do
      it "parses new line" $ do
        parsesChar "'\\n'" '\n'
      it "parses carriage return" $ do
        parsesChar "'\\r'" '\r'
      it "parses form feed" $ do
        parsesChar "'\\f'" '\f'
      it "parses backspace" $ do
        parsesChar "'\\b'" '\b'
      it "parses tab" $ do
        parsesChar "'\\t'" '\t'
      it "parses single quote" $ do
        parsesChar "'\\''" '\''
      it "parses double quote" $ do
        parsesChar "'\\\"'" '"'
      it "parses backslash" $ do
        parsesChar "'\\\\'" '\\'
      it "parses null" $ do
        parsesChar "'\0'" '\NUL'
    context "when given an invalid escape character" $ do
      it "fails to parse" $ do
        charLit `failsToParse` "'\\z'"
    context "when given an valid 4 digit hex character" $ do
      it "parses the hex character" $ do
        mapM_ (uncurry parsesChar) [ ("'\\x0061'", 'a')
                                   , ("'\\x0041'", 'A')
                                   , ("'\\x0030'", '0')
                                   , ("'\\x0039'", '9')
                                   , ("'\\x0046'", 'F')
                                   , ("'\\xaBcD'", '\xabcd')
                                   ]
    context "when given an invalid 4 digit hex character" $ do
      it "fails to parse" $ do
        charLit `failsToParse` "'\\x00z0'"
    context "when given an invalid length hex character" $ do
      it "fails to parse" $ do
        mapM_ (failsToParse charLit) [ "'\\x0'"
                                     , "'\\x'"
                                     , "'\\x000'"
                                     , "'\\x00000'"
                                     ]
    context "when given an empty character literal" $ do
      it "fails to parse" $ do
        charLit `failsToParse` "''"
    context "when given a character literal with no closing quote" $ do
      it "fails to parse" $ do
        charLit `failsToParse` "'a"
  
  describe "strLit" $ do
    context "when given a string literal" $ do
      it "parses the string" $ do
        parsesString "\"hello\"" "hello"
    context "when given a string with escaped characters" $ do
      it "parses the string" $ do
        parsesString "\"\\n\\r\\f\\b\\t\\'\\\"\\\\\\0\"" "\n\r\f\b\t'\"\\\0"
    context "when given a string with escaped hex characters" $ do
      it "parses the string" $ do
        parsesString "\"\\x0061\\x0041\\x0030\\x0039\\x0046\\xabcd\"" "aA09F\xabcd"
    context "when given a string with invalid escaped characters" $ do
      it "fails to parse" $ do
        strLit `failsToParse` "\"\\z\""
    context "when given a string with invalid escaped hex characters" $ do
      it "fails to parse" $ do
        strLit `failsToParse` "\"\\x00z0\""
    context "when given an empty string literal" $ do
      it "parses the empty string" $ do
        parsesString "\"\"" ""
    context "when given a string with no closing quote" $ do
      it "fails to parse" $ do
        strLit `failsToParse` "\"hello"