{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Shexkell.Semantic.Validation where

import Shexkell.Control.Validation
import Shexkell.Data.Common
import Shexkell.Data.ShEx
import Shexkell.Data.TripleExpr
import Shexkell.Semantic.NodeConstraint (satisfies2)
import Shexkell.Semantic.Neighbourhood
import Shexkell.Semantic.Partition

import Data.RDF

import Data.Foldable (asum)
import Data.Composition
import Data.String
import Data.Maybe
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace


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

----------------
-- satisfiesM --
----------------

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
  -- Get the graph
  gr <- reader graph

  -- If there's a expression, find the partition that matches it
  part <- maybe (return Nothing) (\expr ->
    partitionM (`matches` expr) (Set.fromList $ neigh gr node)) expression

  let (matched, remainder) = fromMaybe (Set.empty, Set.fromList $ neigh gr node) part
  let outs = remainder `Set.intersection` Set.fromList (arcsOut gr node)
  let matchables = maybe Set.empty (\expr -> Set.filter (containsPredicate expr) outs) expression
  let unmatchables = outs `Set.difference` matchables
  
  sm <- case expression of
    Just expr -> not <$> anyM (`singleMatch` expr) (Set.toList matchables)
    Nothing   -> return True

  return $ fromMaybe True (allM (inExtra extra . predicateOf) (Set.toList matchables))
    && (maybe True not closed || Set.null unmatchables) && sm


satisfiesM node (ShapeRef label) = do
  sch <- reader schema
  case findShapeByLabel label sch of
    Just justExpr -> satisfiesM node justExpr
    _             -> return False


inExtra :: Maybe [IRI] -> Node -> Maybe Bool
inExtra extra (UNode iri) = elem iri . map fromString <$> extra
inExtra _     _           = Just False


containsPredicate :: TripleExpr -> Triple -> Bool
containsPredicate TripleConstraint{..} (Triple _ (UNode iri) _) = iri == fromString predicate
containsPredicate _                    _              = False


anyM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
anyM p = fmap or . mapM p

allM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
allM p = fmap and . mapM p

-------------
-- matches --
-------------

matches :: (
    Rdf gr
  , MonadState ShapeMap m
  , MonadReader (ValidationContext gr) m
  ) =>
     Set.Set Triple
  -> TripleExpr
  -> m Bool
matches triples tripleExpr
  | isJust (cardMin tripleExpr) || isJust (cardMax tripleExpr) = let
    fromMin Nothing = 1
    fromMin (Just n) = n
    fromMax Nothing = 1
    fromMax (Just Star) = length triples
    fromMax (Just (IntMax n)) = n
  in
    isJust . asum <$> mapM
      (partitionN (allM (`matches` withoutCardinality tripleExpr)) triples)
      [fromMin (cardMin tripleExpr)..fromMax (cardMax tripleExpr)]
    

matches triples OneOf{..} = anyM (matches triples) expressions

matches triples EachOf{..} =
  isJust <$> partitionN (fmap and . zipWithM (flip matches) expressions) triples (length expressions)

matches triples expr@TripleConstraint{}
  | Set.size triples == 1 = head (Set.elems triples) `singleMatch` expr
  | otherwise = return False

singleMatch :: (
    Rdf gr
  , MonadReader (ValidationContext gr) m
  , MonadState ShapeMap m
  ) =>
     Triple
  -> TripleExpr
  -> m Bool
singleMatch t@(Triple s (UNode iri) o) TripleConstraint{..} = do
  let inv = fromMaybe False inverse
  gr <- reader graph

  let value = if inv then s else o
  let getArcs = if inv then arcsIn else arcsOut
  let arcs = getArcs gr value


  let predicateMatches = iri == fromString predicate

  valueMatches <- maybe (return True) (satisfiesM value) valueExpr

  return $ predicateMatches && valueMatches && maybe True (const $ t `elem` arcs) inverse
