{-# LANGUAGE RecordWildCards #-}

module Shexkell.TestSuite where


import Data.Aeson
import Data.RDF
import Data.Either.Utils
import Data.String
import Data.Foldable (forM_)

import Shexkell.TestSuite.Data.Types
import qualified Test.HUnit as T

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import Prelude hiding (id)
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans

import Shexkell.Text
import Shexkell.Data.ShEx
import Shexkell.Data.Common

import Shexkell.Semantic.Validation

import Debug.Trace


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
fromTestCase TestConfiguration{..} tcase@TestCase{..} = T.TestCase $ testEither expected $ do
  -- Read the contents of the ShEx schema
  schemaFile <- lift $ readFile $ basePath ++ "/validation/" ++ schemaPath

  -- Parse the graph
  graph      <- lift (parseFile (TurtleParser Nothing Nothing) (basePath ++ "/validation/" ++ graphPath) :: IO (Either ParseFailure (RDF TList)))
  gr         <- hoistEither $ setLeft ParseGraph graph

  -- Parse the schema
  schema     <- hoistEither $ setLeft ParseShex $ parseShex schemaFile CompactShexParser

  -- Get the validations to perform
  Just queryMap <- lift $ getValidationMap basePath tcase
  -- Iterate through them
  forM_ (M.toList queryMap) $ \(n, s) -> do
    -- Get the shape to validate from the test case
    sh   <- maybeToEither ShapeNotFound $ findShapeByLabel (toShapeLabel s) schema
    node <- hoistEither $ findNode (toNode n) gr

    -- Get the function to validate based on the test case
    let validate = case expected of
                  ValidationTest -> satisfies
                  ValidationFailure -> notSatisfies

    -- Run the validation
    lift $ T.assertBool ("Unexpected validation result at " ++ show id) $ validate schema gr (ShapeMap M.empty) sh node


-- Find a node in the graph given its ID
findNode :: Rdf gr => Node -> RDF gr -> Either TestSuiteException Node
findNode node graph = maybeToEither NodeNotFound $ queryLeft <|> queryMid <|> queryRight
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

toShapeLabel :: String -> ShapeLabel
toShapeLabel ('_':(':':bnodeLbl)) = BNodeId bnodeLbl
toShapeLabel lbl = IRILabel lbl

--------------------------------------------
-- * Validation map
--------------------------------------------

getValidationMap ::
     String                               -- ^ Base path to search the file map
  -> TestCase                             -- ^ Test case to get the map from
  -> IO (Maybe (M.Map QueryNode String))
getValidationMap basePath tcase@TestCase{..} = runMaybeT $
  -- Try to get the map from the file if it's specified in the test case
  (M.mapKeys QueryUNode <$> (getMapFromFile . ((basePath ++ "/validation/") ++) =<< maybeT testMap)) <|>
  -- Otherwise get it from the test case itself
  getMapFromCase tcase

-- | Reads the map file and parses it from JSON
getMapFromFile :: String -> MaybeT IO (M.Map String String)
getMapFromFile mapPath = do
  mapFile <- lift $ B.readFile mapPath
  maybeT $ decode mapFile

-- | If the test case specifies a node and shape to validate, builds a map
--   from it
getMapFromCase :: TestCase -> MaybeT IO (M.Map QueryNode String)
getMapFromCase TestCase{..} = do
  sh <- maybeT shape
  node <- maybeT focus
  return $ M.singleton node sh 

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return

---------------------------------------------


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
  show ParseShex = "Error parsing shape expression"
  show ParseGraph = "Error parsing graph"

setLeft :: c -> Either a b -> Either c b
setLeft y (Left _)  = Left y
setLeft _ (Right x) = Right x