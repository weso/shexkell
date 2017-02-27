module Shexkell.Text.Compact.Common where

import Shexkell.Data.Common

import Text.ParserCombinators.Parsec
import Debug.Trace

iri :: Parser IRI
iri = prefixedName <|>
      between (symbol '<') (symbol '>') (many1 (alphaNum <|> oneOf ":/.#-")) <?> "iri"

shapeLabel :: Parser ShapeLabel
shapeLabel = IRILabel <$> iri

prefixedName :: Parser IRI
prefixedName = try (pnameLn <|> pnameNs)

pnameLn :: Parser IRI
pnameLn = do
  pre <- pnameNs
  local <- many1 alphaNum <* spaces
  return (pre ++ local)

pnameNs :: Parser String
pnameNs = many1 alphaNum <* char ':'


symbol :: Char -> Parser Char
symbol sym = char sym <* spaces
