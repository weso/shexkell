{-# LANGUAGE RecordWildCards #-}

module Shexkell.Semantic.Validation where

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

satisfies :: Rdf graph =>
     Node
  -> ShapeExpr
  -> RDF graph
  -> ShapeMap
  -> Bool
satisfies node shex@NodeConstraint{} gr m = satisfies2 shex node
satisfies node (ShapeOr _ exprs) gr m  = any (\expr -> satisfies node expr gr m) exprs
satisfies node (ShapeAnd _ exprs) gr m = all (\expr -> satisfies node expr gr m) exprs
satisfies node (ShapeNot _ expr) gr m  = not $ satisfies node expr gr m

satisfies node Shape{..} gr m = let
  (matched, remainder) = fromMaybe ([], neigh gr node) ((\expr -> partition (match expr m) (neigh gr node)) <$> expression)
  outs = Set.fromList remainder `Set.intersection` Set.fromList (arcsOut gr node)
  matchables = fromMaybe Set.empty ((\expr -> Set.filter (match expr m) outs) <$> expression)
  in False



notSatisfies :: Rdf graph =>
     Node
  -> ShapeExpr
  -> RDF graph
  -> ShapeMap
  -> Bool
notSatisfies = not .:: satisfies


data ValidationContext gr = ValidationContext {
    schema :: Schema
  , graph :: RDF gr
}

type Validation gr = Reader (ValidationContext gr) Bool

askSchema :: Reader (ValidationContext gr) Schema
askSchema = schema <$> ask

askGraph :: Reader (ValidationContext gr) (RDF gr)
askGraph = graph <$> ask

runValidation :: (Rdf gr) => Schema -> RDF gr -> Validation gr -> Bool
runValidation schema gr val = runReader val (ValidationContext schema gr)

satisfiesM :: (Rdf gr) => Node -> ShapeExpr -> ShapeMap -> Validation gr
satisfiesM node shex@NodeConstraint{} m = return $ satisfies2 shex node

satisfiesM node (ShapeOr _ exprs) m  = do
  gr <- askGraph
  anyM (\expr -> satisfiesM node expr m) exprs

satisfiesM node (ShapeAnd _ exprs) m = do
  gr <- askGraph
  allM (\expr -> satisfiesM node expr m) exprs

satisfiesM node (ShapeNot _ expr) m = do
  gr <- askGraph
  not <$> satisfiesM node expr m

satisfiesM node Shape{..} m = do
  gr <- askGraph
  let (matched, remainder) = fromMaybe ([], neigh gr node) ((\expr -> partition (match expr m) (neigh gr node)) <$> expression)
  let outs = Set.fromList remainder `Set.intersection` Set.fromList (arcsOut gr node)
  let matchables = fromMaybe Set.empty ((\expr -> Set.filter (match expr m) outs) <$> expression)
  return False

satisfiesM node (ShapeRef label) m = do
  schema <- askSchema
  let expr = shapes schema >>= find ((== Just label) . shexId)
  case expr of
    Just justExpr -> satisfiesM node justExpr m
    _             -> return False


anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p t = or <$> mapM p t

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p t = and <$> mapM p t
