module Shexkell.Data.ShapeExpr where

import Shexkell.Data.Common (IRI, SemAct, ObjectValue)


data NodeKind =
    IRIKind
  | BNodeKind
  | NonLiteralKind
  | LiteralKind
  deriving Show

data XsFacet =
    XsStringFacet  StringFacet
  | XsNumericFacet NumericFacet
  deriving Show

data StringFacet =
    LitStringFacet     String Int
  | PatternStringFacet String String
  deriving Show

data NumericFacet =
    MinInclusive   String NumericLiteral
  | MinExclusive   String NumericLiteral
  | MaxInclusive   String NumericLiteral
  | MaxExclusive   String NumericLiteral
  | TotalDigits    String Int
  | FractionDigits String Int
  deriving Show

data NumericLiteral =
    NumericInt     Int
  | NumericDouble  Double
  | NumericDecimal Double
  deriving Show


data ValueSetValue =
    ObjectValue ObjectValue
  | Stem        IRI
  | StemRange   StemValue (Maybe [ValueSetValue])
  deriving Show

data StemValue =
    IRIStem IRI
  | Wildcard
  deriving Show

instance Eq NumericLiteral where
  (NumericInt n1) == (NumericInt n2) = n1 == n2
  (NumericDouble d1) == (NumericDouble d2) = d1 == d2
  (NumericDecimal d1) == (NumericDecimal d2) = d1 == d2

  (NumericInt n) == (NumericDouble d) = fromIntegral n == d
  (NumericInt n) == (NumericDecimal d) = fromIntegral n == d

  (NumericDouble d) == (NumericInt n) = d == fromIntegral n
  (NumericDouble d1) == (NumericDecimal d2) = d1 == d2

  (NumericDecimal d) == (NumericInt n) = d == fromIntegral n
  (NumericDecimal d1) == (NumericDouble d2) = d1 == d2

instance Ord NumericLiteral where
  (NumericInt n1) `compare` (NumericInt n2) = n1 `compare` n2
  (NumericDouble d1) `compare` (NumericDouble d2) = d1 `compare` d2
  (NumericDecimal d1) `compare` (NumericDecimal d2) = d1 `compare` d2

  (NumericInt n) `compare` (NumericDouble d) = fromIntegral n `compare` d
  (NumericInt n) `compare` (NumericDecimal d) = fromIntegral n `compare` d

  (NumericDouble d) `compare` (NumericInt n) = d `compare` fromIntegral n
  (NumericDouble d) `compare` (NumericDecimal d2) = d `compare` d2

  (NumericDecimal d) `compare` (NumericInt n) = d `compare` fromIntegral n
  (NumericDecimal d1) `compare` (NumericDouble d2) = d1 `compare` d2
