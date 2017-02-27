module Main where

import System.Environment

import Shexkell.Text.Compact.ShexParser (parse, shexDoc)

main :: IO ()
main = do
    [path] <- getArgs
    re path


re :: String -> IO ()
re path = do
  contents <- readFile path
  case parse shexDoc "Prueba" contents of
      Left err -> print err
      Right success -> print success
