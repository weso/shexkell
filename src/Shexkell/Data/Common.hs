module Shexkell.Data.Common where

import Data.Char (toLower)


data SemAct = SemAct IRI (Maybe String)
  deriving Show

data ObjectValue =
    IRIValue IRI
  | StringValue String
  | DatatypeString String IRI
  | LangString String String
  deriving Show

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
