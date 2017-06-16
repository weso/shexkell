module Shexkell.SemanticTests.PartitionTest (
  partitionTests
) where

import Test.HUnit

import qualified Data.Set as Set
import Shexkell.Semantic.Partition

import Control.Monad.Identity


partitionTests :: Test
partitionTests = TestList [
    testFound
  , testNotFound
  , testFoundOneRight

  , testPartitionN1
  , testPartitionN2
  , testPartitionSingleton
  ]


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

testFoundOneRight :: Test
testFoundOneRight = TestCase $ assertEqual
  "Test successful partition. One element on the right"
  (Just (Set.fromList [1,2,3,4,5], Set.fromList [6]))
  (partition (all (<= 5)) (Set.fromList [1, 2, 3, 4, 5, 6]))

testNotFound :: Test
testNotFound = TestCase $ assertEqual
  "Test unsuccessful partition"
  Nothing
  (partition (== testPredNotFound) testSet)


-----------------------------
-- * Partition N -------------
-----------------------------

testData1 :: Set.Set Int
testData1 = Set.fromList [1..5]

runPartitionN :: (Ord a, Show a) =>
     ([Set.Set a] -> Bool)
  -> Set.Set a
  -> Int
  -> Maybe [Set.Set a]
runPartitionN p set = runIdentity . partitionN (return . p) set 

testPartitionN1 :: Test
testPartitionN1 = TestCase $ assertEqual
  "Test succesful partitionN 1"
  (Just [Set.fromList [2, 4], Set.fromList [1, 3, 5]])
  (runPartitionN evensLeftOddsRight testData1 2)
  where
    evensLeftOddsRight [left, right] = all even left && all odd right
    evensLeftOddsRight _ = False

testPartitionN2 :: Test    
testPartitionN2 = TestCase $ assertEqual
  "Test succesful partitionN 2"
  (Just [Set.empty, Set.fromList [1, 2, 3, 4, 5]])
  (runPartitionN leftEmpty testData1 2)
  where leftEmpty [left, right] = Set.null left && right == Set.fromList [1, 2, 3, 4, 5]

testPartitionSingleton :: Test
testPartitionSingleton = TestCase $ assertEqual
  "Test successful partitionN singleton"
  (Just [Set.singleton (3 :: Int)])
  (runPartitionN is3 (Set.singleton 3) 1)
  where
    is3 [s] = case Set.toList s of [3] -> True; _ -> False
    is3 _ = False


