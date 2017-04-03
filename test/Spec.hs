import Shexkell.SemanticTests
import Shexkell.TestSuite
import Shexkell.TestSuite.Data.Types ()

import Test.HUnit

import System.Environment (getArgs)
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Control.Monad


main :: IO ()
main = do
  args <- getArgs
  void $ runTestTT =<< case args of
    -- If no args passed, run unit tests
    [] -> putStrLn "\nRunning unit tests...\n" >> return unitTests

    -- If compat arg is passed, run compatibility tests with the specified configuration
    ["--compat", confFilePath] -> do
      putStrLn "\nRunning compatibility tests...\n"
      confFile <- B.readFile confFilePath
      case eitherDecode confFile of
        Left err   -> error $ "Error reading test configuration: " ++ show err
        Right conf -> loadTests conf
      
      
unitTests :: Test
unitTests = semanticTests