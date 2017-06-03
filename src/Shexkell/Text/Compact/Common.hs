module Shexkell.Text.Compact.Common where

import Shexkell.Text.Compact.Control

import Shexkell.Data.Common

import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec)

import Control.Monad (void)


iri :: ParserShex IRI
iri = prefixedName <|>
      withBase (between (symbol '<') (symbol '>') (many1 (alphaNum <|> oneOf ":/.#-")) <?> "iri")

bnode :: Parsec String u String
bnode = string "_:" >> many1 alphaNum <* skippeables <?> "bnode"   

shapeLabel :: ParserShex ShapeLabel
shapeLabel = (IRILabel <$> iri) <|> (BNodeId <$> bnode)

prefixedName :: ParserShex IRI
prefixedName = try (pnameLn <|> pnameNs)

pnameLn :: ParserShex IRI
pnameLn = do
  pre   <- pnameNs
  local <- many alphaNum <* skippeables
  withPrefix pre local

pnameNs :: Parsec String u String
pnameNs = (many alphaNum <* char ':') <* skippeables


symbol :: Char -> Parsec String u Char
symbol sym = char sym <* skippeables

keyword :: String -> Parsec String u String
keyword str = string str <* skippeables

skippeables :: Parsec String u ()
skippeables = between spaces spaces (void parseComment <|> spaces)

parseComment :: Parsec String u String
parseComment = (char '#' >> manyTill anyChar (void (many1 newline) <|> eof)) <?> "comment"