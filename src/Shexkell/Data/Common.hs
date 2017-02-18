module Shexkell.Data.Common where

import Control.DeepSeq (NFData, rnf)

data SemAct = SemAct IRI (Maybe String)

data ObjectValue =
    IRIValue IRI
  | StringValue String
  | DatatypeString String IRI
  | LangString String String

type IRI = String

data ShapeLabel =
    IRILabel IRI
  | BNodeId String
  deriving Eq


instance NFData SemAct where
  rnf (SemAct iri str) = rnf iri `seq` rnf str

instance NFData ObjectValue where
  rnf (IRIValue iri) = rnf iri
  rnf (StringValue str) = rnf str
  rnf (DatatypeString str iri) = rnf str `seq` rnf iri
  rnf (LangString a b) = rnf a `seq` rnf b

instance NFData ShapeLabel where
  rnf (IRILabel iri) = rnf iri
  rnf (BNodeId str) = rnf str

instance Ord ShapeLabel where
  (IRILabel iri) `compare` (IRILabel iri2) = iri `compare` iri2
  (BNodeId str) `compare` (BNodeId str2) = str `compare` str2

  (IRILabel _) `compare` (BNodeId _) = GT
  (BNodeId _) `compare` (IRILabel _) = LT
