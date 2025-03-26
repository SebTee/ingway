{- |
Module      : ParseBCG
Description : Ingway language parser library
Copyright   : (c) Sebastian Tee, 2024
License     : GPL-3.0-or-later
Maintainer  : SebTee

Library for parsing the Ingway language.

This documentaion will use [Extended Backus–Naur form]
(https://en.wikipedia.org/wiki/Extended_Backus-Naur_form)
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

any = ? any character ? ;

space = ? white space character ? ;

spaces = { space } ;

spaces1 = space , spaces ;
@
-}
module Text.Ingway.Parser
  ( -- * Expressions
    Expression(..)
  , expression
  , funcApp 
    -- * Identifiers
  , Ident
  , ident
    -- * Literals
  , Literal(..)
  , literal
    -- ** Numbers
  , numberLit
    -- ** Strings
  , strLit
    -- ** Characters
  , charLit
  , escapedChar
  ) where

import Text.Parsec
import Data.Maybe (fromJust)

data Expression = Lit Literal -- ^ Literal value
                | Var Ident -- ^ Variable identifier
                | App Expression Expression -- ^ Function application
                deriving (Show, Eq)

{- | 
Expression parser.

=== EBNF
@
expression = 'funcApp' | 'literal' | 'ident' ;
@

=== __Examples__
>>> parse expression "" "123"
Right (Lit (NumLit (123 % 1)))
>>> parse expression "" "\"hello\""
Right (Lit (StrLit "hello"))
>>> parse expression "" "'a'"
Right (Lit (CharLit 'a'))
>>> parse expression "" "add 1 2"
Right (App (App (Var "add") (Lit (NumLit (1 % 1)))) (Lit (NumLit (2 % 1))))
-}
expression :: Parsec String u Expression
expression = try funcApp <|> termExpr

{- | 
Function application parser.

=== EBNF
@
funcApp = 'expression' , spaces1 , 'expression';
@
-}
funcApp :: Parsec String u Expression
funcApp = do
  f <- termExpr
  skipMany1 space
  a <- expression
  case a of
    App f' a' -> return $ App (App f f') a'
    _ -> return $ App f a

termExpr :: Parsec String u Expression
termExpr = Lit <$> literal <|> Var <$> ident

-- | A variable or type identifier.
type Ident = String

{- | 
Parse an identifier.

=== EBNF
@ident = letter , { letter | digit | "_" } ;@

=== __Examples__
>>> parse ident "" "hello"
Right "hello"
>>> parse ident "" "hello123"
Right "hello123"
>>> parse ident "" "hello_123"
Right "hello_123"
>>> parse ident "" "HelloWorld"
Right "HelloWorld"
>>> parse ident "" "123Hello" -- can't start with a number
Left (line 1, column 1):
unexpected "1"
expecting letter
>>> parse ident "" "_HelloWorld" -- can't start with an underscore
Left (line 1, column 1):
unexpected "_"
expecting letter
-}
ident :: Parsec String u Ident
ident = (:) <$> letter <*> many (letter <|> digit <|> char '_')

-- | A literal value in the Ingway language.
data Literal = NumLit Rational
             | StrLit String
             | CharLit Char
             deriving (Show, Eq)

{- |
Parse a literal value. 

=== EBNF
@literal = 'strLit' | 'charLit' | 'numberLit' ;@
-}
literal :: Parsec String u Literal
literal = choice [ StrLit <$> strLit
                 , CharLit <$> charLit
                 , NumLit <$> numberLit
                 ]

{- | 
Parse a number literal. The number is represented as a 'Rational' number.

=== EBNF
@
numberLit = [ "-" ] , digits                     (* Integer part  *)
          , [ "." , digits ]                     (* Fraction part *)
          , [ ( "e" , \"E\" ) , [ "-" ] , digits ] (* Exponent part *) ;

digits = digit , { digit } ;
@

=== __Examples__
>>> parse numberLit "" "123"
Right (NumLit (123 % 1))
>>> parse numberLit "" "12.3"
Right (NumLit (123 % 10))
>>> parse numberLit "" "12.3e0"
Right (NumLit (123 % 10))
>>> parse numberLit "" "-12.3E3"
Right (NumLit ((-12300) % 1))
>>> parse numberLit "" "-12.3e4"
Right (NumLit ((-123000) % 1))
>>> parse numberLit "" "-12.3E-3"
Right (NumLit ((-123) % 10000))
>>> parse numberLit "" "0.0"
Right (NumLit (0 % 1))
>>> parse numberLit "" "0000.000"
Right (NumLit (0 % 1))
>>> parse numberLit "" "0.0001"
Right (NumLit (1 % 10000))
-}
numberLit :: Parsec String u Rational
numberLit = do
  s <- fromInteger <$> maybeNeg :: Parsec String u Rational
  i <- fromInteger <$> uInt
  f <- option "0" $ char '.' *> digits
  let f' = fromInteger (read f) / (10 ^^ length f)
  e <- option (0 :: Integer) $ oneOf "eE" *>
    ((*) <$> maybeNeg) <*> uInt
  return $ s * (i + f') * (10 ^^ e)
  where
    uInt = read <$> digits
    maybeNeg = option 1 ((-1) <$ char '-')
    digits = many1 digit

{- |
Parse a string literal.

=== EBNF
@charLit = """ , ( 'escapedChar' | any - """ - "\\" ) , """ ;@

=== __Examples__
>>> parse strLit "" "\"hello\""
Right (StrLit "hello")
>>> parse strLit "" "\"\\\"\""
Right (StrLit "\"")
-}
strLit :: Parsec String u String
strLit = between pqm pqm (many $ escapedChar <|> noneOf [qm])
  where
    pqm = char qm
    qm = '\"'

{- |
Parse a character literal.

=== EBNF
@charLit = "'" , ( 'escapedChar' | any - "'" - "\\" ) , "'" ;@

=== __Examples__
>>> parse charLit "" "'a'"
Right (CharLit 'a')
>>> parse charLit "" "'\\n'"
Right (CharLit '\n')
>>> parse charLit "" "'\\x0041'"
Right (CharLit 'A')
-}
charLit :: Parsec String u Char
charLit = between pqm pqm (escapedChar <|> noneOf [qm])
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
