{-# LANGUAGE RecordWildCards #-}

module Shexkell.Semantic.Shape where

import Data.RDF (Triple, Triples, Node)
import qualified Data.Set as Set

import Shexkell.Data.ShEx

matches ::
     Triples
  -> TripleExpr
  -> ShapeMap
  -> Bool
matches triples OneOf{..} m = any (\expr -> matches triples expr m) expressions


match :: TripleExpr -> ShapeMap -> Triple -> Bool
match OneOf{..} m triple = any (\expr -> match expr m triple) expressions
