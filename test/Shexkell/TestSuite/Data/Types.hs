{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shexkell.TestSuite.Data.Types where

import Data.Aeson
import Data.Text (unpack)


import Prelude hiding (id)


-- | Configuration of the tests to run
data TestConfiguration = TestConfiguration {

    basePath     :: String           -- ^ Base path where the test suite is located
  , manifestPath :: String           -- ^ Name of the manifest file
  , testCases    :: Maybe [String]   -- ^ IDs of the test cases to run

}

----------------------------------------
-- * Test suite data model
----------------------------------------

data TestCase = TestCase {
    id         :: String
  , schemaPath :: String
  , graphPath  :: String
  , shape      :: Maybe String
  , focus      :: Maybe QueryNode
  , testMap    :: Maybe String
  , expected   :: ExpectedResult
}

newtype TestManifest = TestManifest {
  cases :: [TestCase]
}

data QueryNode =
    QueryUNode String
  | QueryTypedLiteral String String
  deriving (Eq, Ord)

data ExpectedResult = ValidationTest | ValidationFailure


--------------------
-- * JSON parsing
--------------------

instance FromJSON TestManifest where
  parseJSON = withObject "Manifest" $ \ m -> do
    [gr]    <- m .: "@graph"
    entries <- gr .: "entries"
    return $ TestManifest entries 


instance FromJSON TestCase where
  parseJSON = withObject "Test case" $ \ o -> do
    id         <- o .: "@id"
    expected   <- o .: "@type"
    action     <- o .: "action"
    schemaPath <- action .: "schema"
    graphPath  <- action .: "data"
    shape      <- action .:? "shape"
    focus      <- action .:? "focus"
    testMap    <- action .:? "map"
    return TestCase{..}

instance FromJSON QueryNode where
  parseJSON (String uri) = return $ QueryUNode (unpack uri)
  parseJSON (Object o) = do
    value <- o .: "@value"
    ty    <- o .: "@type"
    return $ QueryTypedLiteral value ty

  parseJSON _ = fail "Expected string or object"

instance FromJSON ExpectedResult where
  parseJSON (String "sht:ValidationTest")    = return ValidationTest
  parseJSON (String "sht:ValidationFailure") = return ValidationFailure
  parseJSON _                                = fail "Expected sht:ValidationTest or sht:ValidationFailure"

instance FromJSON TestConfiguration where
  parseJSON = withObject "Test configuration" $ \ o -> do
    basePath     <- o .:  "basePath"
    manifestPath <- o .:  "manifestPath"
    testCases    <- o .:? "casesToRun"
    return TestConfiguration{..}
