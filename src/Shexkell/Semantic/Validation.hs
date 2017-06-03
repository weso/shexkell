{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Shexkell.Semantic.Validation where

import Shexkell.Control.Validation
import Shexkell.Data.Common
import Shexkell.Data.ShEx
import Shexkell.Data.ShapeMap
import Shexkell.Data.TripleExpr
import Shexkell.Semantic.NodeConstraint (satisfies2)
import Shexkell.Semantic.Neighbourhood
import Shexkell.Semantic.Partition

import Data.RDF

import Data.Foldable (asum)
import Data.Composition
import Data.String
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State


-- | Perform the validation of a given graph agains a schema using a given Shape Map
validateMap :: Rdf graph =>
     Schema                  
  -> RDF graph
  -> ShapeMap
  -> ShapeMap
validateMap schema graph shapeMap@(ShapeMap inputMap) = execValidation
  -- For every node in the Shape Map
  (forM_ (Map.toList inputMap) $ \(node, shaps) ->
    -- For every shape to validate the node against
    forM_ shaps $ \ (shape, _) ->
      -- Perform the validation
      satisfiesM node shape) (ValidationContext graph schema) shapeMap


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
satisfiesM node shex@NodeConstraint{} = updateValidation node shex $ satisfies2 shex node

satisfiesM node shex@(ShapeOr _ exprs)  = anyM (satisfiesM node) exprs >>= updateValidation node shex

satisfiesM node shex@(ShapeAnd _ exprs) = allM (satisfiesM node) exprs >>= updateValidation node shex

satisfiesM node shex@(ShapeNot _ expr) = not <$> satisfiesM node expr >>= updateValidation node shex

satisfiesM node shex@Shape{..} = do
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

  updateValidation node shex $ fromMaybe True (allM (inExtra extra . predicateOf) (Set.toList matchables))
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
containsPredicate EachOf{..} tripl = any (`containsPredicate` tripl) expressions
containsPredicate OneOf{..} tripl = any (`containsPredicate` tripl) expressions
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
    fromMin Nothing = 0
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

  if not predicateMatches then
    return False
  else do
    valueMatches <- maybe (return True) (satisfiesM value) valueExpr
    return $ valueMatches && maybe True (const $ t `elem` arcs) inverse
