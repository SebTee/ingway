module Ingway.Parse.Token 
  ( Expression (..)
  , Ident (..)
  , Literal (..)
  ) where

-- | An expression can be evaluated into vale.
--   A value may be a function.
data (Eq a, Show a) => Expression a
  -- | An expression where the first expression must evaluate to a function 
  --   that will be applied to the second expression.
  = Exp
    (Expression a) -- ^ Evaluates to a function.
    (Expression a) -- ^ Evaluates to a value which is passed into the function.
  | Lit Literal -- ^ A literal value.
  | Id (Ident a) -- ^ Identifier that refrences a value.
  deriving (Show, Eq)

-- | Identifier that is used to reference a value.
newtype (Eq a, Show a) => Ident a = Ident a
  deriving (Show, Eq)

-- | Literal value represented in the code.
data Literal
  = I Integer -- ^ An integer.
  | F Double -- ^ A floating point number.
  | S String -- ^ A string of characters.
  | C Char -- ^ A Character.
  deriving (Show, Eq)