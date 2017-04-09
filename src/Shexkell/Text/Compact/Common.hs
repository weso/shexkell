module Shexkell.Text.Compact.Common where

import Shexkell.Text.Compact.Control

import Shexkell.Data.Common

import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec)

import Debug.Trace

iri :: ParserShex IRI
iri = prefixedName <|>
      withBase (between (symbol '<') (symbol '>') (many1 (alphaNum <|> oneOf ":/.#-")) <?> "iri")

bnode :: Parsec String u String
bnode = string "_:" >> many1 alphaNum <* spaces <?> "bnode"   

shapeLabel :: ParserShex ShapeLabel
shapeLabel = (IRILabel <$> iri) <|> (BNodeId <$> bnode)

prefixedName :: ParserShex IRI
prefixedName = try (pnameLn <|> pnameNs)

pnameLn :: ParserShex IRI
pnameLn = do
  pre   <- pnameNs
  local <- many alphaNum <* spaces
  withPrefix pre local

pnameNs :: Parsec String u String
pnameNs = (many alphaNum <* char ':') <* spaces


symbol :: Char -> Parsec String u Char
symbol sym = char sym <* spaces

keyword :: String -> Parsec String u String
keyword str = string str <* spaces
