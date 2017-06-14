module Shexkell.Data.Common where

import Data.Char (toLower)


data SemAct = SemAct IRI (Maybe String)
  deriving (Show, Eq)

data ObjectValue =
    IRIValue IRI
  | StringValue String
  | DatatypeString String IRI
  | LangString String String
  | NumericValue NumericLiteral
  deriving (Show, Eq)

data NumericLiteral =
    NumericInt     Int
  | NumericDouble  Double
  | NumericDecimal Double
  deriving (Show)

type IRI = String

data ShapeLabel =
    IRILabel IRI
  | BNodeId String
  deriving (Show)

modifyLabel :: (String -> String) -> ShapeLabel -> ShapeLabel
modifyLabel f (IRILabel iri) = IRILabel $ f iri
modifyLabel f (BNodeId bnodeId) = BNodeId $ f bnodeId

instance Eq ShapeLabel where
  (IRILabel a) == (IRILabel b) = map toLower a == map toLower b
  (BNodeId a) == (BNodeId b) = map toLower a == map toLower b
  _ == _ = False

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
