{-# LANGUAGE RecordWildCards #-}

{-
Module : CommandLine 
Description : Simple Command Line Parser tool

Simple data type and typeclass for parsing command line arguments to obtain
the options of the program
-}
module CommandLine (
    CLIOptions(..)

  , Parameter
  , Flag
) where

import Control.Monad (foldM)


-------------------------------------------------
-- * Command Line Options data model
-------------------------------------------------

-- | Abstract representation of the options of the program
data CommandLineOptions = CommandLineOptions {
    parameters :: [Parameter] -- ^ Key -> value parameters
  , flags      :: [Flag]      -- ^ Flags given to the program
} deriving Show

-- | (Key, Value)
type Parameter = (String, String)

type Flag = String


-------------------------------------------------
-- * Command Line parsing
-------------------------------------------------

parseArgs :: [String] -> Either String CommandLineOptions
parseArgs = fmap fst <$> foldM accumulate (CommandLineOptions [] [], Nothing)
  where
    accumulate (clo@CommandLineOptions{..}, Nothing) ('-':('-':flag)) =
      Right (clo { flags = flag:flags }, Nothing)

    accumulate (clo@CommandLineOptions{}, Nothing) ('-':paramKey) =
      Right (clo, Just paramKey)
    accumulate (_, Just _) ('-':paramKey) =
      Left $ "Expected parameter value, got key " ++ paramKey
    accumulate (clo@CommandLineOptions{..}, Just paramKey) paramValue =
      Right (clo { parameters = (paramKey, paramValue):parameters }, Nothing)


-------------------------------------------------
-- * CLIOptions type class
-------------------------------------------------

-- | Type class to build command line options from the program arguments
class CLIOptions a where
  -- | Obtain the options from a list of arguments
  fromArgs :: [String] -> Either String a
  fromArgs args = do
    CommandLineOptions{..} <- parseArgs args
    return $ foldr addParam (foldr addFlag dflt flags) parameters

  -- | Initial data
  dflt :: a
  -- | Register a flag
  addFlag :: Flag -> a -> a
  -- | Register a parameter
  addParam :: Parameter -> a -> a
