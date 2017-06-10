{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Debug.Trace

-- | Perform the validation of a given graph agains a schema using a given Shape Map
validateMap :: Rdf graph =>
     Schema                  
  -> RDF graph
  -> ShapeMap
  -> ShapeMap
validateMap schema graph im@(ShapeMap inputMap) = combineShapeMaps im $ execValidation
  -- For every node in the Shape Map
  (forM_ (Map.toList inputMap) $ \(node, shaps) ->
    -- For every shape to validate the node against
    forM_ shaps $ \ (shape, _) -> 
      -- Perform the validation
      void $ satisfiesM node shape
      ) (ValidationContext graph schema) (ShapeMap Map.empty)


satisfies :: Rdf graph =>
     Schema
  -> RDF graph
  -> ShapeMap
  -> ShapeExpr
  -> Node
  -> Bool
satisfies sch graph smap shex node = 
  runValidation (satisfiesM node shex) (ValidationContext graph sch) (ShapeMap Map.empty)

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
  , MonadState ValidationState m)
    =>
      Node
   -> ShapeExpr
   -> m Bool
satisfiesM node shex@NodeConstraint{} = checkNode node shex $ return $ satisfies2 shex node

satisfiesM node shex@(ShapeOr _ exprs)  = checkNode node shex $
  anyM (satisfiesM node) exprs

satisfiesM node shex@(ShapeAnd _ exprs) = checkNode node shex $
  allM (satisfiesM node) exprs 

satisfiesM node shex@(ShapeNot _ expr) = checkNode node shex $
  not <$> satisfiesM node expr

satisfiesM node shex@Shape{..} = checkNode node shex $ do
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
    Just expr -> not . or <$> sequence
      [triple `singleMatch` constraint | triple <- Set.toList matchables
                                       , constraint <- tripleConstraintsOf expr]
    Nothing   -> return True

  return $ sm && fromMaybe True (allM (inExtra extra . predicateOf) (Set.toList matchables))
    && (maybe True not closed || Set.null unmatchables)


satisfiesM node shape@(ShapeRef label) = checkNode node shape $ do
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

tripleConstraintsOf :: TripleExpr -> [TripleExpr]
tripleConstraintsOf constr@TripleConstraint{..} = [constr]
tripleConstraintsOf EachOf{..} = findTripleConstraints expressions
tripleConstraintsOf OneOf{..} = findTripleConstraints expressions

findTripleConstraints :: [TripleExpr] -> [TripleExpr]
findTripleConstraints = filter isTripleConstraint


anyM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
anyM p = fmap or . mapM p

allM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
allM p = fmap and . mapM p

-------------
-- matches --
-------------

matches :: (
    Rdf gr
  , MonadState ValidationState m
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
  , MonadState ValidationState m
  ) =>
     Triple
  -> TripleExpr
  -> m Bool
singleMatch t@(Triple s (UNode iri) o) TripleConstraint{..} = do
  gr <- reader graph
  (Just node) <- currentNode <$> get

  let inv = fromMaybe False inverse
  let value = if inv then s else o
  let getArcs = if inv then arcsIn else arcsOut
  let arcs = getArcs gr node

  let predicateMatches = iri == fromString predicate

  if not (t `elem` arcs && predicateMatches) then
    return False
  else 
    maybe (return True) (satisfiesM value) valueExpr

singleMatch _ _ = return False