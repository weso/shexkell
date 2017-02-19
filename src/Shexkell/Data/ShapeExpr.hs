module Shexkell.Data.ShapeExpr where

import Shexkell.Data.Common (IRI, SemAct, ObjectValue)

import Control.DeepSeq (NFData, rnf)

data NodeKind =
    IRIKind
  | BNodeKind
  | NonLiteralKind
  | LiteralKind
  deriving Show

data XsFacet =
    XsStringFacet  StringFacet
  | XsNumericFacet NumericFacet

data StringFacet =
    LitStringFacet     String Int
  | PatternStringFacet String String

data NumericFacet =
    MinInclusive   String NumericLiteral
  | MinExclusive   String NumericLiteral
  | MaxInclusive   String NumericLiteral
  | MaxExclusive   String NumericLiteral
  | TotalDigits    String Int
  | FractionDigits String Int

data NumericLiteral =
    NumericInt     Int
  | NumericDouble  Double
  | NumericDecimal Double


data ValueSetValue =
    ObjectValue ObjectValue
  | Stem        IRI
  | StemRange   StemValue (Maybe [ValueSetValue])

data StemValue =
    IRIStem IRI
  | Wildcard

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


instance NFData ValueSetValue where
  rnf (ObjectValue ov) = rnf ov
  rnf (Stem iri) = rnf iri
  rnf (StemRange val vs) = rnf val `seq` rnf vs

instance NFData StemValue where
  rnf (IRIStem iri) = rnf iri
  rnf Wildcard = ()

instance NFData XsFacet where
  rnf (XsStringFacet str) = rnf str
  rnf (XsNumericFacet num) = rnf num

instance NFData StringFacet where
  rnf (LitStringFacet field n) = rnf field `seq` rnf n
  rnf (PatternStringFacet field pat) = rnf field `seq` rnf pat

instance NFData NumericFacet where
  rnf (MinInclusive field n) = rnf field `seq` rnf n
  rnf (MinExclusive field n) = rnf field `seq` rnf n
  rnf (MaxInclusive field n) = rnf field `seq` rnf n
  rnf (MaxExclusive field n) = rnf field `seq` rnf n
  rnf (TotalDigits field n) = rnf field `seq` rnf n
  rnf (FractionDigits field n) = rnf field `seq` rnf n

instance NFData NumericLiteral where
  rnf (NumericInt n) = rnf n
  rnf (NumericDouble n) = rnf n
  rnf (NumericDecimal n) = rnf n
