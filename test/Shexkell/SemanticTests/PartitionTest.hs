module Shexkell.SemanticTests.PartitionTest (
  partitionTests
) where

import Test.HUnit

import qualified Data.Set as Set
import Shexkell.Semantic.Partition

partitionTests :: Test
partitionTests = TestList [testFound, testNotFound]

testSet :: Set.Set Int
testSet = Set.fromList [1..10]

testPredFound :: Set.Set Int
testPredFound = Set.fromList [1, 4, 6, 9]

testPredNotFound :: Set.Set Int
testPredNotFound = Set.fromList [-5, 15, 25]

expectedPartition :: Set.Set Int -> Set.Set Int -> Maybe (Set.Set Int, Set.Set Int)
expectedPartition expected testSet = Just (expected, testSet `Set.difference` expected)

testFound :: Test
testFound = TestCase $ assertEqual
  "Test successful partition"
  (expectedPartition testPredFound testSet)
  (partition (== testPredFound) testSet)

testNotFound :: Test
testNotFound = TestCase $ assertEqual
  "Test unsuccessful partition"
  Nothing
  (partition (== testPredNotFound) testSet)
