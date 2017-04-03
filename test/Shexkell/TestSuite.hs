{-# LANGUAGE RecordWildCards #-}

module Shexkell.TestSuite where


import Data.Aeson
import Data.RDF
import Data.Either.Utils
import Data.String

import Shexkell.TestSuite.Data.Types
import qualified Test.HUnit as T

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import Prelude hiding (id)
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.Trans

import Shexkell.Text
import Shexkell.Data.ShEx
import Shexkell.Data.Common

import Shexkell.Semantic.Validation


-- | Load the manifest file from the configuration and create the test to run
loadTests :: TestConfiguration -> IO T.Test
loadTests conf@TestConfiguration{..} = do
  -- Load and parse the manifest
  manifest <- fmap eitherDecode . B.readFile $ basePath ++ "/validation/" ++ manifestPath

  -- If there's a parse error, fail the test. Otherwise, build a composite test with
  -- all the test cases that are specified to run in the configuration
  case manifest of
    Left err -> return $ T.TestCase $ T.assertFailure $ "Failed parsing manifest: " ++ err 
    Right TestManifest{..} ->
      let shouldTest = maybe (const True) (flip elem) testCases in
        return $ T.TestList $ map (fromTestCase conf) (filter (shouldTest . id) cases)


-- | Create a test from a test case of the manifest
fromTestCase :: TestConfiguration -> TestCase -> T.Test
fromTestCase TestConfiguration{..} TestCase{..} = T.TestCase $ testEither expected $ do
  -- Read the contents of the ShEx schema
  schemaFile <- lift $ readFile $ basePath ++ "/validation/" ++ schemaPath
  -- Parse the graph
  graph      <- lift (parseFile (TurtleParser Nothing Nothing) (basePath ++ "/validation/" ++ graphPath) :: IO (Either ParseFailure (RDF TList)))
  -- Parse the schema
  schema     <- hoistEither $ setLeft ParseShex $ parseShex schemaFile CompactShexParser
  -- Get the shape to validate from the test case
  sh         <- case shape of
    Just shId -> maybeToEither ShapeNotFound $ findShapeByLabel (IRILabel shId) schema
    Nothing   -> maybeToEither NoShapeStart (start schema)

  -- Get the node to validate from the test case and the loaded graph
  gr <- hoistEither $ setLeft ParseGraph graph
  node       <- hoistEither $ findNode (toNode focus) gr

  -- Get the function to validate based on the test case
  let validate = case expected of
                ValidationTest -> satisfies
                ValidationFailure -> notSatisfies

  -- Run the validation
  lift $ T.assertBool "Unexpected validation result" $ validate schema gr (ShapeMap M.empty) sh node

  return ()


-- Find a node in the graph given its ID
findNode :: Rdf gr => Node -> RDF gr -> Either TestSuiteException Node
findNode node graph = case queryLeft <|> queryMid <|> queryRight of
  Nothing -> Left NodeNotFound
  Just n  -> Right n
  where
    queryLeft = queryGraph pickLeft (Just node) Nothing Nothing
    queryMid  = queryGraph pickMid Nothing (Just node) Nothing
    queryRight = queryGraph pickRight Nothing Nothing (Just node)

    pickLeft (Triple l _ _) = l
    pickMid (Triple _ m _) = m
    pickRight (Triple _ _ r) = r

    queryGraph s l m r = case query graph l m r of
      [] -> Nothing
      (tr:_) -> Just $ s tr


-- | Run an EitherT monad. If it returns a left, check if the test case specifies that the
--   test must or must not fail.
testEither :: ExpectedResult -> EitherT TestSuiteException IO () -> IO ()
testEither expected me = runEitherT me >>= either (exceptionToTest expected) return where
  exceptionToTest ValidationTest except = T.assertFailure (show except)
  exceptionToTest ValidationFailure _   = return ()


toNode :: QueryNode -> Node
toNode (QueryUNode uri) = unode (fromString uri)
toNode (QueryTypedLiteral value ty) = lnode $ typedL (fromString value) (fromString ty)

-- | Possible exceptions that may happen during the tests
data TestSuiteException =
    NodeNotFound
  | ShapeNotFound
  | NoShapeStart
  | ParseShex
  | ParseGraph

instance Show TestSuiteException where  
  show NodeNotFound = "Node not found"
  show ShapeNotFound = "Shape not found"
  show NoShapeStart = "No shape specified in start"
  show ParseShex = "Error parsing shape expression"
  show ParseGraph = "Error parsing graph"

setLeft :: c -> Either a b -> Either c b
setLeft y (Left _)  = Left y
setLeft _ (Right x) = Right x