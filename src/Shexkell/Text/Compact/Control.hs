module Shexkell.Text.Compact.Control where

import Data.RDF.Namespace (PrefixMappings(..))
import Data.Text (pack, unpack)
import qualified Data.Map as M

import Text.Parsec hiding (State)

import Shexkell.Data.Common


type ParserShex a = Parsec String ParsingContext a

data ParsingContext = ParsingContext {
    currentBase :: IRI
  , prefixes    :: PrefixMappings
}


-- | Parses a shape expression with compact syntax
parseShexC :: ParserShex a -> SourceName -> String -> Either ParseError a
parseShexC parser = runParser parser $ ParsingContext [] (PrefixMappings M.empty)


-- | Sets the current base
putBase :: IRI -> ParserShex IRI
putBase iri = do
  modifyState $ \ ctx -> ctx { currentBase = iri }
  return iri

-- | Adds a prefix to the prefix mappings of the schema
putPrefix ::
     String        -- ^ Prefix
  -> String        -- ^ IRI that corresponds the prefix
  -> ParserShex ()
putPrefix prefix mapping = modifyState $ \ ctx@(ParsingContext _ (PrefixMappings pmap)) ->
  ctx { prefixes = PrefixMappings (M.insert (pack prefix) (pack mapping) pmap) }

-- | Given a parser that parses an IRI, uses the current base to absolutize
--   that IRI
withBase :: ParserShex IRI -> ParserShex IRI
withBase p = do
  iri <- p
  base <- currentBase <$> getState
  return $ case base of
    [] -> iri
    _  -> base ++ iri

-- | Given a prefix and an identifier, obtains the IRI that corresponds the
--   prefix from the parsing state and creates an absolute IRI
withPrefix :: String -> String -> ParserShex IRI 
withPrefix pre local = do
  PrefixMappings pmap <- prefixes <$> getState
  case M.lookup (pack pre) pmap of
    Just prefix -> return $ unpack prefix ++ local
    Nothing     -> fail "Prefix not found"