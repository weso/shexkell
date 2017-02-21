{-# LANGUAGE RecordWildCards #-}

module Shexkell.Control.DeepSeq where

import Control.DeepSeq

import Shexkell.Data.Common
import Shexkell.Data.ShEx
import Shexkell.Data.ShapeExpr
import Shexkell.Data.TripleExpr

instance NFData ShapeExpr where
  rnf NodeConstraint{..} =
    rnf dataType `seq`
    rnf xsFacets `seq`
    rnf values
  rnf Shape{..} =
    rnf virtual `seq`
    rnf closed `seq`
    rnf extra `seq`
    rnf expression `seq`
    rnf inherit `seq`
    rnf semActs
  rnf (ShapeOr lbl exprs) = rnf lbl `seq` rnf exprs
  rnf (ShapeAnd lbl exprs) = rnf lbl `seq` rnf exprs
  rnf (ShapeNot lbl expr) = rnf lbl `seq` rnf expr
  rnf (ShapeRef lbl) = rnf lbl
  rnf _ = ()

instance NFData TripleExpr where
  rnf EachOf{..} =
    rnf expressions `seq`
    rnf cardMin `seq`
    rnf cardMax `seq`
    rnf triplSemActs `seq`
    rnf annotations
  rnf OneOf{..} =
    rnf expressions `seq`
    rnf cardMin `seq`
    rnf cardMax `seq`
    rnf triplSemActs `seq`
    rnf annotations
  rnf TripleConstraint{..} =
    rnf inverse `seq`
    rnf negated `seq`
    rnf predicate `seq`
    rnf valueExpr `seq`
    rnf cardMin `seq`
    rnf cardMax `seq`
    rnf triplSemActs `seq`
    rnf annotations
  rnf (Inclusion lbl) = rnf lbl

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

instance NFData Annotation where
  rnf (Annotation iri ov) = rnf iri `seq` rnf ov

instance NFData Max where
  rnf Star = ()
  rnf (IntMax n) = rnf n

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
