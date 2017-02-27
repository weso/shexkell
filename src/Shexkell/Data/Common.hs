module Shexkell.Data.Common where

import Control.DeepSeq (NFData, rnf)

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
  deriving (Eq, Show)
