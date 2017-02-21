module Shexkell (
    Schema(..)
  , ShapeExpr(..)
  , TripleExpr(..)
  , ShapeMap(..)
  , shexId

  , triplMin
  , triplMax


  , SemAct(..)
  , ObjectValue(..)
  , IRI
  , ShapeLabel(..)

  , NodeKind(..)
  , XsFacet(..)
  , StringFacet(..)
  , NumericFacet(..)
  , NumericLiteral(..)
  , ValueSetValue(..)
  , StemValue(..)

  , Annotation(..)
  , Max(..)

) where

import Shexkell.Data.Common
import Shexkell.Data.ShEx
import Shexkell.Data.ShapeExpr
import Shexkell.Data.TripleExpr

import Control.DeepSeq
import Shexkell.Control.Validation
import Shexkell.Control.DeepSeq
