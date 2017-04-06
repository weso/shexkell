module Shexkell.Text.Compact.Control where

import Text.Parsec hiding (State)

import Shexkell.Data.Common

type ParserShex a = Parsec String IRI a


-- | Parses a shape expression with compact syntax
parseShexC :: ParserShex a -> SourceName -> String -> Either ParseError a
parseShexC parser = runParser parser ""


-- | Sets the current base
putBase :: IRI -> ParserShex IRI
putBase iri = do
  putState iri
  return iri

-- | Given a parser that parses an IRI, uses the current base to absolutize
--   that IRI
withBase :: ParserShex IRI -> ParserShex IRI
withBase p = do
  iri <- p
  currentBase <- getState
  return $ case currentBase of
    [] -> iri
    _  -> currentBase ++ iri