module Shexkell.Data.ShapeExpr where

import Shexkell.Data.Common (IRI, SemAct, ObjectValue, NumericLiteral)


data NodeKind =
    IRIKind
  | BNodeKind
  | NonLiteralKind
  | LiteralKind
  deriving (Show, Eq)

data XsFacet =
    XsStringFacet  StringFacet
  | XsNumericFacet NumericFacet
  deriving (Show, Eq)

data StringFacet =
    LitStringFacet     String Int
  | PatternStringFacet String String
  deriving (Show, Eq)

data NumericFacet =
    MinInclusive   String NumericLiteral
  | MinExclusive   String NumericLiteral
  | MaxInclusive   String NumericLiteral
  | MaxExclusive   String NumericLiteral
  | TotalDigits    String Int
  | FractionDigits String Int
  deriving (Show, Eq)




data ValueSetValue =
    ObjectValue ObjectValue
  | Stem        StemValue
  | StemRange   StemValue (Maybe [ValueSetValue])
  deriving (Show, Eq)

data StemValue =
    IRIStem IRI
  | LiteralStem ObjectValue
  | Wildcard
  deriving (Show, Eq)

