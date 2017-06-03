module Main where

import System.Environment

import Shexkell.Text

import Data.Aeson hiding (parseJSON)
import Data.String
import Shexkell.Data.ShEx
import Shexkell.Text.JSON.ShexParser ()
import Shexkell
import qualified Data.ByteString.Lazy.Char8 as B


main :: IO ()
main = do
  (shapeMap:(graph:(schema:_))) <- getArgs
  result <- validate (ShexOptions CompactFormat TurtleFormat) shapeMap graph schema
  B.putStrLn $ encode result
   


mainJSON :: IO ()
mainJSON = do
  [path] <- getArgs
  parsed <- parseJSON path
  case parsed of
    Left err -> print err
    Right sch -> print sch


re :: String -> IO ()
re path = do
  contents <- readFile path
  case parseShex contents CompactShexParser of
      Left err -> print err
      Right success -> print success

parseJSON :: String -> IO (Either String Schema)
parseJSON fileName = eitherDecode . fromString <$> readFile fileName
