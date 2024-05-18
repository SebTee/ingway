{- |
Module      : ParseBCG
Description : Ingway language parser library
Copyright   : (c) Sebastian Tee, 2024
License     : GPL-3.0-or-later
Maintainer  : SebTee

Library for parsing the Ingway language.

This bocumentaion will use
[Extended Backus–Naur form](https://en.wikipedia.org/wiki/Extended_Backus-Naur_form)
(EBNF) to describe the grammar of the language.

=== __Basic EBNF definitions__
@
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;

upperCase = \"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\"
          | \"G\" | \"H\" | \"I\" | \"J\" | \"K\" | \"L\" 
          | \"M\" | \"N\" | \"O\" | \"P\" | \"Q\" | \"R\" 
          | \"S\" | \"T\" | \"U\" | \"V\" | \"W\" | \"X\" 
          | \"Y\" | \"Z\" ;

lowerCase = \"a\" | \"b\" | \"c\" | \"d\" | \"e\" | \"f\"
          | \"g\" | \"h\" | \"i\" | \"j\" | \"k\" | \"l\" 
          | \"m\" | \"n\" | \"o\" | \"p\" | \"q\" | \"r\" 
          | \"s\" | \"t\" | \"u\" | \"v\" | \"w\" | \"x\" 
          | \"y\" | \"z\" ;

letter = upperCase | lowerCase ;

hexDigit = digit
         | \"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\" 
         | "a" | "b" | "c" | "d" | "e" | "f" ;

anyChar = ? any character ? ;
@
-}
module Text.Ingway.Parser where

import Text.Parsec
import Data.Maybe (fromJust)

-- * Literals

-- | A literal value in the Ingway language.
data Literal = IntLit Integer
             | FloatLit Double
             | StrLit String
             | CharLit Char
             deriving (Show, Eq)

-- ** Strings

{- |
Parse a string literal.

=== EBNF
@charLit = """ , ( 'escapedChar' | anyChar - """ - "\\" ) , """ ;@

=== __Examples__
>>> parse strLit "" "\"hello\""
Right (StrLit "hello")
>>> parse strLit "" "\"\\\"\""
Right (StrLit "\"")
-}
strLit :: Parsec String u Literal
strLit = StrLit <$> between pqm pqm (many $ escapedChar <|> noneOf [qm])
  where
    pqm = char qm
    qm = '\"'

-- ** Characters

{- |
Parse a character literal.

=== EBNF
@charLit = "'" , ( 'escapedChar' | anyChar - "'" - "\\" ) , "'" ;@

=== __Examples__
>>> parse charLit "" "'a'"
Right (CharLit 'a')
>>> parse charLit "" "'\\n'"
Right (CharLit '\n')
>>> parse charLit "" "'\\x0041'"
Right (CharLit 'A')
-}
charLit :: Parsec String u Literal
charLit = CharLit <$> between pqm pqm (escapedChar <|> noneOf [qm])
  where
    pqm = char qm
    qm = '\''

{- |
Parse an escaped character into a single character.
@"\\"@ is the escape character.

==== __Single character escape__
+--------+---------+-----------------+
| Escape | Unicode | Character       |
+========+=========+=================+
| @\\0@  | U+0000  | Null character  |
+--------+---------+-----------------+
| @\\b@  | U+0008  | Backspace       |
+--------+---------+-----------------+
| @\\t@  | U+0009  | Horizontal tab  |
+--------+---------+-----------------+
| @\\n@  | U+000A  | New line        |
+--------+---------+-----------------+
| @\\f@  | U+000C  | Form feed       |
+--------+---------+-----------------+
| @\\r@  | U+000D  | Carriage return |
+--------+---------+-----------------+
| @\\"@  | U+0022  | Double quote    |
+--------+---------+-----------------+
| @\\'@  | U+0027  | Single quote    |
+--------+---------+-----------------+
| @\\\\@ | U+005C  | Backslash       |
+--------+---------+-----------------+

==== Four hexadecimal digit escape characters
@\\xHHHH@ where @H@ is a hexadecimal digit.

=== EBNF
@
escapedChar = "\\" , ( singleCharEscape | charHex ) ;
singleCharEscape = """ | "'" | "\\" | "b" | "f" | "n" | "r" | "t" | "0" ;
charHex = "x" , 4 * hexDigit ;
@

=== __Examples__
>>> parse escapedChar "" "\n"
Right '\n'
>>> parse escapedChar "" "\x0041"
Right 'A'
>>> parse escapedChar "" "\0"
Right '\NUL'
-}
escapedChar :: Parsec String u Char
escapedChar = do
  _ <- char '\\'
  c <- oneOf $ hexChar : map fst charMap
  if c == hexChar
    then charHex
    else return $ fromJust $ lookup c charMap
  where
    hexChar = 'x'
    charMap = [ ('\"', '\"')
              , ('\'', '\'')
              , ('\\', '\\')
              , ('b', '\b')
              , ('f', '\f')
              , ('n', '\n')
              , ('r', '\r')
              , ('t', '\t')
              , ('0', '\NUL')
              ]
    charHex = do
      c <- count 4 hexDigit
      return $ toEnum $ read $ "0x" ++ c
