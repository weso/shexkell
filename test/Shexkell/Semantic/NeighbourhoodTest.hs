{-# LANGUAGE OverloadedStrings #-}

module Shexkell.Semantic.NeighbourhoodTest
  (
    neighbourhoodTests
  ) where

import Shexkell.Semantic.Neighbourhood

import Data.RDF
import Test.HUnit hiding (Node)

neighbourhoodTests :: Test
neighbourhoodTests = TestList [testArcsOut1, testArcsOut2]

nodeA = bnode "a"
nodeB = bnode "b"
nodeC = bnode "c"

predAB = unode "ab"
predAC = unode "ac"
predBC = unode "bc"

tripleAB = triple nodeA predAB nodeB
tripleAC = triple nodeA predAC nodeC
tripleBC = triple nodeB predBC nodeC

testGraph1 :: RDF TList
testGraph1 = addTriples (empty :: RDF TList) [tripleAB, tripleAC, tripleBC]

testArcsOut1 :: Test
testArcsOut1 = TestCase $ assertBool "Arcs out from A" (triplesMatch (arcsOut testGraph1 nodeA) [tripleAB, tripleAC])

testArcsOut2 :: Test
testArcsOut2 = TestCase $ assertBool "Arcs out from B" (triplesMatch (arcsOut testGraph1 nodeB) [tripleBC])




addTriples :: Rdf gr => RDF gr -> Triples -> RDF gr
addTriples = foldl addTriple

triplesMatch :: Triples -> Triples -> Bool
triplesMatch actual expected = all (`elem` expected) actual && (length actual == length expected)
