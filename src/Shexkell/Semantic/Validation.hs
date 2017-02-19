{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Shexkell.Semantic.Validation where

import Shexkell.Control.Validation
import Shexkell.Data.ShEx
import Shexkell.Semantic.Shape
import Shexkell.Semantic.NodeConstraint (satisfies2)
import Shexkell.Semantic.Neighbourhood

import Data.RDF

import Data.Composition
import Data.String
import Data.Maybe
import Data.List (partition, find)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.State

satisfies :: Rdf graph =>
     Schema
  -> RDF graph
  -> ShapeMap
  -> ShapeExpr
  -> Node
  -> Bool
satisfies sch graph smap shex node =
  runValidation (satisfiesM node shex) (ValidationContext graph sch) smap

notSatisfies :: Rdf graph =>
     Schema
  -> RDF graph
  -> ShapeMap
  -> ShapeExpr
  -> Node
  -> Bool
notSatisfies = not .::. satisfies


satisfiesM :: (
    Rdf gr
  , MonadReader (ValidationContext gr) m
  , MonadState ShapeMap m)
    =>
      Node
   -> ShapeExpr
   -> m Bool
satisfiesM node shex@NodeConstraint{} = return $ satisfies2 shex node

satisfiesM node (ShapeOr _ exprs)  = anyM (satisfiesM node) exprs

satisfiesM node (ShapeAnd _ exprs) = allM (satisfiesM node) exprs

satisfiesM node (ShapeNot _ expr) = not <$> satisfiesM node expr

satisfiesM node Shape{..} = do
  gr <- reader graph
  m <- get
  let (matched, remainder) = fromMaybe ([], neigh gr node) ((\expr -> partition (match expr m) (neigh gr node)) <$> expression)
  let outs = Set.fromList remainder `Set.intersection` Set.fromList (arcsOut gr node)
  let matchables = fromMaybe Set.empty ((\expr -> Set.filter (match expr m) outs) <$> expression)
  let unmatchables = outs `Set.difference` matchables
  return False

satisfiesM node (ShapeRef label) = do
  schShapes <- reader $ shapes . schema
  let expr = schShapes >>= find ((== Just label) . shexId)
  case expr of
    Just justExpr -> satisfiesM node justExpr
    _             -> return False


anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = fmap or . mapM p

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = fmap and . mapM p
