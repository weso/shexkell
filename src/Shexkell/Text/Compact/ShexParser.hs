{-# LANGUAGE RecordWildCards #-}

module Shexkell.Text.Compact.ShexParser (
    module Text.ParserCombinators.Parsec
  , shexDoc
) where

import Shexkell.Data.ShEx
import Shexkell.Data.Common
import Shexkell.Data.TripleExpr

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

shexDoc :: Parser Schema
shexDoc = do
  dir <- many directive
  st <- optionMaybe  notStartAction
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

statement :: Parser (Either Directive ShapeExpr)
statement = (Left <$> directive) <|> (Right <$> shexDecl)


notStartAction :: Parser ShapeExpr
notStartAction = parseStart-- <|> shexDecl

shexDecl :: Parser ShapeExpr
shexDecl = do
  lbl <- shapeLabel
  expr <- parseShapeExpr
  return $ setLabel lbl expr

directive :: Parser Directive
directive = (Base <$> baseDecl) <|> (Prefix <$> prefixDecl)

parseStart :: Parser ShapeExpr
parseStart = keyword "start" >> symbol '=' >> parseShapeExpr

baseDecl :: Parser IRI
baseDecl = keyword "BASE" >> iri

prefixDecl :: Parser PrefixMapping
prefixDecl = do
  keyword "PREFIX" >> spaces
  pname <- pnameNs <* spaces
  iriref  <- iri
  return $ PrefixMapping (fromString pname, fromString iriref)

-----------------------------------
-- * Shape Expression
-----------------------------------

parseShapeExpr :: Parser ShapeExpr
parseShapeExpr = shapeOr

shapeOr :: Parser ShapeExpr
shapeOr = compositeShape shapeAnd "OR" (ShapeOr Nothing)

shapeAnd :: Parser ShapeExpr
shapeAnd = compositeShape shapeNot "AND" (ShapeAnd Nothing)

shapeNot :: Parser ShapeExpr
shapeNot = do
  isNot <- isJust <$> optionMaybe (keyword "NOT" <* spaces)
  shape <- shapeAtom
  return $ if isNot then ShapeNot Nothing shape else shape

shapeAtom :: Parser ShapeExpr
shapeAtom =
    withOpt nodeConstraint shapeOrRef (ShapeAnd Nothing) <|>
    shapeOrRef <|>
    between (symbol '(') (symbol ')') parseShapeExpr <|>
    (empty <$ symbol '.')

shapeOrRef :: Parser ShapeExpr
shapeOrRef = shapeDefinition <|>
             ShapeRef <$> (char '@' *> shapeLabel)

shapeDefinition :: Parser ShapeExpr
shapeDefinition = do
  eoc <- many extraOrClosed
  expr <- between (symbol '{') (symbol '}') (optionMaybe tripleExpr)
  -- annotation
  let closed = listToMaybe $ rights eoc
  let extras = mconcat $ lefts eoc

  return $ Shape Nothing Nothing closed (Just extras) expr Nothing Nothing

extraOrClosed :: Parser (Either [IRI] Bool)
extraOrClosed = (Right True <$ keyword "CLOSED") <|>
                Left <$> (keyword "EXTRA" >> many1 iri)

inlineShapeExpr :: Parser ShapeExpr
inlineShapeExpr = inlineShapeOr

inlineShapeOr :: Parser ShapeExpr
inlineShapeOr = compositeShape inlineShapeAnd "OR" (ShapeOr Nothing)

inlineShapeAnd :: Parser ShapeExpr
inlineShapeAnd = compositeShape inlineShapeNot "AND" (ShapeAnd Nothing)

inlineShapeNot :: Parser ShapeExpr
inlineShapeNot = do
  isNot <- isJust <$> optionMaybe (keyword "NOT" >> spaces)
  sh <- inlineShapeAtom
  return $ if isNot then ShapeNot Nothing sh else sh

inlineShapeAtom :: Parser ShapeExpr
inlineShapeAtom = withOpt nodeConstraint inlineShapeOrRef (ShapeAnd Nothing) <|>
                  withOpt inlineShapeOrRef nodeConstraint (ShapeAnd Nothing) <|>
                  between (symbol '(') (symbol ')') parseShapeExpr <|>
                  empty <$ (symbol '.' >> spaces)

inlineShapeOrRef :: Parser ShapeExpr
inlineShapeOrRef = inlineShapeDefinition <|>
                   ShapeRef <$> (char '@' *> shapeLabel)

inlineShapeDefinition :: Parser ShapeExpr
inlineShapeDefinition = do
  eoc <- many extraOrClosed
  expr <- between (symbol '{') (symbol '}') (optionMaybe tripleExpr)
  let closed = not $ null (rights eoc)
  let extras = mconcat $ lefts eoc

  return $ Shape Nothing Nothing (Just closed) (Just extras) expr Nothing Nothing

withOpt ::
     Parser ShapeExpr
  -> Parser ShapeExpr
  -> ([ShapeExpr] -> ShapeExpr)
  -> Parser ShapeExpr
withOpt parseFirst parseOpt constructor = do
  first <- parseFirst
  opt   <- optionMaybe parseOpt
  return $ case opt of
    Just opt' -> constructor [first, opt']
    Nothing   -> first

compositeShape ::
     Parser ShapeExpr
  -> String
  -> ([ShapeExpr] -> ShapeExpr)
  -> Parser ShapeExpr
compositeShape leaf word constructor = do
  sh <- leaf
  shs <- many $ keyword word >> leaf
  return $ case shs of
    [] -> sh
    _  -> constructor (sh:shs)


--------------------------------------
-- * Triple Expression
--------------------------------------

tripleExpr :: Parser TripleExpr
tripleExpr = oneOfTripleExpr

oneOfTripleExpr :: Parser TripleExpr
oneOfTripleExpr = groupTripleExpr <|> multiElementOneOf

groupTripleExpr :: Parser TripleExpr
groupTripleExpr = multiElementGroup <|> singleElementGroup

innerTripleExpr :: Parser TripleExpr
innerTripleExpr = multiElementOneOf <|> multiElementGroup

multiElementOneOf :: Parser TripleExpr
multiElementOneOf = do
  left <- groupTripleExpr
  rights <- many (symbol '|' >> groupTripleExpr)
  return $  case rights of
    [] -> left
    _  -> OneOf (left:rights) Nothing Nothing Nothing Nothing

singleElementGroup :: Parser TripleExpr
singleElementGroup = unaryTripleExpr <* optional (symbol ';')

multiElementGroup :: Parser TripleExpr
multiElementGroup = do
  left <- unaryTripleExpr
  rights <- many (symbol ';' >> unaryTripleExpr)
  optional $ symbol ';'
  return $ case rights of
    [] -> left
    _  -> EachOf (left:rights) Nothing Nothing Nothing Nothing

unaryTripleExpr :: Parser TripleExpr
unaryTripleExpr = (tripleConstraint <|> bracketedTripleExpr) <|>
                  include

tripleConstraint :: Parser TripleExpr
tripleConstraint = do
  inv <- senseFlag
  p   <- parsePredicate
  value <- inlineShapeExpr
  (min, max) <- fromMaybe (Nothing, Nothing) <$> optionMaybe cardinality

  let valueE
        | isEmpty value = Nothing
        | otherwise     = Just value

  return $ TripleConstraint (Just inv) Nothing p valueE min max Nothing Nothing

senseFlag :: Parser Bool
senseFlag = isJust <$> optionMaybe (symbol '^' <* spaces)

parsePredicate :: Parser IRI
parsePredicate = iri

cardinality :: Parser (Maybe Int, Maybe Max)
cardinality = (Nothing, Just Star) <$ symbol '*' <|>
              (Just 1, Just Star) <$ symbol '+' <|>
              (Nothing, Just $ IntMax 1) <$ symbol '?' <|>
              repeatRange

repeatRange :: Parser (Maybe Int, Maybe Max)
repeatRange = do
  symbol '{'
  min <- read <$> many1 digit <* spaces
  max <- optionMaybe $ do
    symbol ','
    (Star <$ symbol '*') <|> (IntMax . read <$> many1 digit <* spaces)
  symbol '}'
  return (Just min, max)


bracketedTripleExpr :: Parser TripleExpr
bracketedTripleExpr = do
  expr <- between (symbol '(') (symbol ')') innerTripleExpr
  (min, max) <- fromMaybe (Nothing, Nothing) <$> optionMaybe cardinality
  return $ case expr of
    EachOf{..} -> EachOf expressions min max triplSemActs annotations
    OneOf{..}  -> OneOf expressions min max triplSemActs annotations
    TripleConstraint{..} -> TripleConstraint inverse negated predicate valueExpr min max triplSemActs annotations


include :: Parser TripleExpr
include = Inclusion <$> (char '&' *> shapeLabel)

-- | For debugging purposes
traceState :: Parser ()
traceState = do
  (State s _ _) <- getParserState
  traceShowM s
