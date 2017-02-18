module Shexkell.Semantic.Neighbourhood
  ( arcsOut
  , arcsIn
  , predicatesOut
  , predicatesIn
  , neigh
  , predicates
  ) where

import Data.RDF (Node, Rdf, RDF, Triples, query, predicateOf)
import qualified Data.Set as Set

arcsOut :: Rdf g =>
     RDF g
  -> Node
  -> Triples
arcsOut graph node = query graph (Just node) Nothing Nothing

predicatesOut :: Rdf g =>
     RDF g
  -> Node
  -> [Node]
predicatesOut graph = map predicateOf . arcsOut graph

arcsIn :: Rdf g =>
     RDF g
  -> Node
  -> Triples
arcsIn graph node = query graph Nothing Nothing (Just node)

predicatesIn :: Rdf g =>
     RDF g
  -> Node
  -> [Node]
predicatesIn graph = map predicateOf . arcsIn graph

neigh :: Rdf g =>
     RDF g
  -> Node
  -> Triples
neigh = unionOf arcsIn arcsOut

predicates :: Rdf g =>
     RDF g
  -> Node
  -> [Node]
predicates = unionOf predicatesIn predicatesOut

unionOf :: (Rdf g, Ord a) =>
     (RDF g -> Node -> [a])
  -> (RDF g -> Node -> [a])
  -> RDF g
  -> Node
  -> [a]
unionOf f g graph node = let
  left  = Set.fromList $ f graph node
  right = Set.fromList $ g graph node
  in Set.toList $ left `Set.union` right
