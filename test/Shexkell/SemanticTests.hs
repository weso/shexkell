module Shexkell.SemanticTests (
  semanticTests
) where

import Test.HUnit

import Shexkell.SemanticTests.NeighbourhoodTest
import Shexkell.SemanticTests.PartitionTest

semanticTests :: Test
semanticTests =
  TestList [
      partitionTests
    , neighbourhoodTests
  ]
