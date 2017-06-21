module Shexkell.Text.Compact.Common where

import Shexkell.Text.Compact.Control

import Shexkell.Data.Common

import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec)

import Data.Char (toLower, toUpper)

import Control.Monad (void)


iri :: ParserShex IRI
iri = prefixedName <|>
      withBase (between (symbol '<') (symbol '>') (many1 (alphaNum <|> oneOf ":/.#-")) <?> "iri") <|>
      typeA

bnode :: Parsec String u String
bnode = string "_:" >> many1 alphaNum <* skippeables <?> "bnode"   

shapeLabel :: ParserShex ShapeLabel
shapeLabel = (IRILabel <$> iri) <|> (BNodeId <$> bnode)

prefixedName :: ParserShex IRI
prefixedName = try (pnameLn <|> pnameNs)

pnameLn :: ParserShex IRI
pnameLn = do
  pre   <- pnameNs
  local <- many (alphaNum <|> oneOf "-") <* skippeables
  withPrefix pre local

pnameNs :: Parsec String u String
pnameNs = (many alphaNum <* char ':') <* skippeables

typeA :: Parsec String u IRI
typeA = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" <$ symbol 'a'

symbol :: Char -> Parsec String u Char
symbol sym = char sym <* skippeables

keyword :: String -> Parsec String u String
keyword str = string str <* skippeables

keyword' :: String -> Parsec String u String
keyword' str = caseInsensitive str <* skippeables

skippeables :: Parsec String u ()
skippeables = between spaces spaces (void parseComment <|> spaces)

parseComment :: Parsec String u String
parseComment = (char '#' >> manyTill anyChar (void (many1 newline) <|> eof)) <?> "comment"

caseInsensitive :: String -> Parsec String u String
caseInsensitive str = str <$ mapM caseInsensitiveChar str where 
  caseInsensitiveChar :: Char -> Parsec String u Char
  caseInsensitiveChar ch = char (toLower ch) <|> char (toUpper ch)