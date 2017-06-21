{-# LANGUAGE RecordWildCards #-}

module Shexkell.Text.Compact.ShexParser (
    module Text.ParserCombinators.Parsec
  , shexDoc
) where

import Shexkell.Data.ShEx
import Shexkell.Data.Common
import Shexkell.Data.TripleExpr

import Shexkell.Text.Compact.Control
import Shexkell.Text.Compact.NodeConstraint
import Shexkell.Text.Compact.Common

import Data.RDF.Namespace (PrefixMapping(..))
import Data.String (fromString)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Either
import Data.Foldable (foldl')

import Text.ParserCombinators.Parsec

import Debug.Trace


data Directive = Base IRI | Prefix PrefixMapping

prefixMappings :: [Directive] -> [PrefixMapping]
prefixMappings = foldl' (\ps d -> case d of
  Prefix pm -> pm:ps
  _         -> ps) []

filterBases :: [Directive] -> [IRI]
filterBases = foldl' (\ bs d -> case d of
  Base i -> i:bs
  _      -> bs) []

----------------------------------------
-- * Schema
----------------------------------------

shexDoc :: ParserShex Schema
shexDoc = do
  skippeables
  dir <- many directive
  st <- optionMaybe  (try notStartAction)
  statements <- many statement
  eof

  let pMappings = prefixMappings dir ++ prefixMappings (lefts statements)
  let bases = filterBases dir ++ filterBases (lefts statements)
  let shapeExprs = rights statements

  return Schema {
      prefixes = Just pMappings
    , base = if null bases then Nothing else Just $ head bases
    , startAct = Nothing
    , start = st
    , shapes = Just shapeExprs
  }

statement :: ParserShex (Either Directive ShapeExpr)
statement = (Left <$> directive) <|> (Right <$> shexDecl)


notStartAction :: ParserShex ShapeExpr
notStartAction = parseStart-- <|> shexDecl

shexDecl :: ParserShex ShapeExpr
shexDecl = do
  lbl <- shapeLabel
  expr <- parseShapeExpr
  return $ setLabel lbl expr

directive :: ParserShex Directive
directive = (Base <$> (baseDecl >>= putBase)) <|> (Prefix <$> prefixDecl)

parseStart :: ParserShex ShapeExpr
parseStart = keyword' "start" >> symbol '=' >> parseShapeExpr

baseDecl :: ParserShex IRI
baseDecl = keyword' "BASE" >> iri

prefixDecl :: ParserShex PrefixMapping
prefixDecl = do
  keyword' "PREFIX"
  pname <- pnameNs 
  iriref  <- iri
  putPrefix pname iriref
  return $ PrefixMapping (fromString pname, fromString iriref)

-----------------------------------
-- * Shape Expression
-----------------------------------

parseShapeExpr :: ParserShex ShapeExpr
parseShapeExpr = shapeOr

shapeOr :: ParserShex ShapeExpr
shapeOr = compositeShape shapeAnd "OR" (ShapeOr Nothing)

shapeAnd :: ParserShex ShapeExpr
shapeAnd = compositeShape shapeNot "AND" (ShapeAnd Nothing)

shapeNot :: ParserShex ShapeExpr
shapeNot = do
  isNot <- isJust <$> optionMaybe (try $ keyword' "NOT")
  shape <- shapeAtom
  return $ if isNot then ShapeNot Nothing shape else shape

shapeAtom :: ParserShex ShapeExpr
shapeAtom =
    withOpt nodeConstraint shapeOrRef (ShapeAnd Nothing) <|>
    shapeOrRef <|>
    between (symbol '(') (symbol ')') parseShapeExpr <|>
    (empty <$ symbol '.')

shapeOrRef :: ParserShex ShapeExpr
shapeOrRef = shapeDefinition <|>
             ShapeRef <$> (char '@' *> shapeLabel)

shapeDefinition :: ParserShex ShapeExpr
shapeDefinition = do
  eoc <- many extraOrClosed
  expr <- between (symbol '{') (symbol '}') (optionMaybe tripleExpr)
  -- annotation
  let closed = listToMaybe $ rights eoc
  let extras = case mconcat $ lefts eoc of
        [] -> Nothing
        ex -> Just ex

  return $ Shape Nothing Nothing closed extras expr Nothing Nothing

extraOrClosed :: ParserShex (Either [IRI] Bool)
extraOrClosed = (Right True <$ keyword' "CLOSED") <|>
                Left <$> (keyword' "EXTRA" >> many1 iri)

inlineShapeExpr :: ParserShex ShapeExpr
inlineShapeExpr = inlineShapeOr

inlineShapeOr :: ParserShex ShapeExpr
inlineShapeOr = compositeShape inlineShapeAnd "OR" (ShapeOr Nothing)

inlineShapeAnd :: ParserShex ShapeExpr
inlineShapeAnd = compositeShape inlineShapeNot "AND" (ShapeAnd Nothing)

inlineShapeNot :: ParserShex ShapeExpr
inlineShapeNot = do
  isNot <- isJust <$> optionMaybe (try $ keyword' "NOT")
  sh <- inlineShapeAtom
  return $ if isNot then ShapeNot Nothing sh else sh

inlineShapeAtom :: ParserShex ShapeExpr
inlineShapeAtom = withOpt nodeConstraint inlineShapeOrRef (ShapeAnd Nothing) <|>
                  withOpt inlineShapeOrRef nodeConstraint (ShapeAnd Nothing) <|>
                  between (symbol '(') (symbol ')') parseShapeExpr <|>
                  empty <$ symbol '.'

inlineShapeOrRef :: ParserShex ShapeExpr
inlineShapeOrRef = inlineShapeDefinition <|>
                   ShapeRef <$> (char '@' *> shapeLabel)

inlineShapeDefinition :: ParserShex ShapeExpr
inlineShapeDefinition = do
  eoc <- many extraOrClosed
  expr <- between (symbol '{') (symbol '}') (optionMaybe tripleExpr)
  let closed = not $ null (rights eoc)
  let extras = mconcat $ lefts eoc

  return $ Shape Nothing Nothing (Just closed) (Just extras) expr Nothing Nothing

withOpt ::
     ParserShex ShapeExpr
  -> ParserShex ShapeExpr
  -> ([ShapeExpr] -> ShapeExpr)
  -> ParserShex ShapeExpr
withOpt parseFirst parseOpt constructor = do
  first <- parseFirst
  opt   <- optionMaybe parseOpt
  return $ case opt of
    Just opt' -> constructor [first, opt']
    Nothing   -> first

compositeShape ::
     ParserShex ShapeExpr
  -> String
  -> ([ShapeExpr] -> ShapeExpr)
  -> ParserShex ShapeExpr
compositeShape leaf word constructor = do
  sh <- leaf
  shs <- many $ keyword' word >> leaf
  return $ case shs of
    [] -> sh
    _  -> constructor (sh:shs)


--------------------------------------
-- * Triple Expression
--------------------------------------

tripleExpr :: ParserShex TripleExpr
tripleExpr = oneOfTripleExpr

oneOfTripleExpr :: ParserShex TripleExpr
oneOfTripleExpr = groupTripleExpr <|> multiElementOneOf

groupTripleExpr :: ParserShex TripleExpr
groupTripleExpr = multiElementGroup <|> singleElementGroup

innerTripleExpr :: ParserShex TripleExpr
innerTripleExpr = multiElementOneOf <|> multiElementGroup

multiElementOneOf :: ParserShex TripleExpr
multiElementOneOf = do
  left <- groupTripleExpr
  rights <- many (symbol '|' >> groupTripleExpr)
  return $  case rights of
    [] -> left
    _  -> OneOf (left:rights) Nothing Nothing Nothing Nothing

singleElementGroup :: ParserShex TripleExpr
singleElementGroup = unaryTripleExpr <* optional (symbol ';')

multiElementGroup :: ParserShex TripleExpr
multiElementGroup = do
  left <- unaryTripleExpr
  rights <- many (try $ symbol ';' >> unaryTripleExpr)
  optional $ symbol ';'
  return $ case rights of
    [] -> left
    _  -> EachOf (left:rights) Nothing Nothing Nothing Nothing

unaryTripleExpr :: ParserShex TripleExpr
unaryTripleExpr = (tripleConstraint <|> bracketedTripleExpr) <|>
                  include

tripleConstraint :: ParserShex TripleExpr
tripleConstraint = do
  inv <- senseFlag
  p   <- parsePredicate
  value <- inlineShapeExpr
  (min, max) <- fromMaybe (Nothing, Nothing) <$> optionMaybe cardinality

  let valueE
        | isEmpty value = Nothing
        | otherwise     = Just value

  return $ TripleConstraint (if inv then Just True else Nothing) Nothing p valueE min max Nothing Nothing

senseFlag :: ParserShex Bool
senseFlag = isJust <$> optionMaybe (symbol '^')

parsePredicate :: ParserShex IRI
parsePredicate = iri

cardinality :: ParserShex (Maybe Int, Maybe Max)
cardinality = (Nothing, Just Star) <$ symbol '*' <|>
              (Just 1, Just Star) <$ symbol '+' <|>
              (Nothing, Just $ IntMax 1) <$ symbol '?' <|>
              repeatRange

repeatRange :: ParserShex (Maybe Int, Maybe Max)
repeatRange = do
  symbol '{'
  min <- read <$> many1 digit <* skippeables
  max <- optionMaybe $ do
    symbol ','
    (Star <$ symbol '*') <|> (IntMax . read <$> many1 digit <* skippeables)
  symbol '}'
  return (Just min, Just $ fromMaybe (IntMax min) max)


bracketedTripleExpr :: ParserShex TripleExpr
bracketedTripleExpr = do
  expr <- between (symbol '(') (symbol ')') innerTripleExpr
  (min, max) <- fromMaybe (Nothing, Nothing) <$> optionMaybe cardinality
  return $ case expr of
    EachOf{..} -> EachOf expressions min max triplSemActs annotations
    OneOf{..}  -> OneOf expressions min max triplSemActs annotations
    TripleConstraint{..} -> TripleConstraint inverse negated predicate valueExpr min max triplSemActs annotations


include :: ParserShex TripleExpr
include = Inclusion <$> (char '&' *> shapeLabel)

-- | For debugging purposes
traceState :: ParserShex ()
traceState = do
  (State s _ _) <- getParserState
  traceShowM s
