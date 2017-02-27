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
import Data.Maybe (isJust, fromMaybe)
import Data.Either

import Text.ParserCombinators.Parsec

import Debug.Trace
import Control.Monad.State (get)

----------------------------------------
-- * Schema
----------------------------------------

shexDoc :: Parser Schema
shexDoc = do
  dir <- directives
  st <- notStartAction
  eof
  let prefixMappings = if null $ rights dir then Nothing else Just $ rights dir
  let bases = if null $ lefts dir then Nothing else Just $ lefts dir

  return Schema {
      prefixes = prefixMappings
    , base = Nothing
    , startAct = Nothing
    , start = Just st
    , shapes = Nothing
  }

notStartAction :: Parser ShapeExpr
notStartAction = parseStart <|> shexDecl

shexDecl :: Parser ShapeExpr
shexDecl = do
  lbl <- shapeLabel
  expr <- parseShapeExpr
  return $ setLabel lbl expr

directives :: Parser [Either IRI PrefixMapping]
directives = many ((Left <$> baseDecl) <|> (Right <$> prefixDecl))

parseStart :: Parser ShapeExpr
parseStart = string "start" >> spaces >> char '=' >> spaces >> parseShapeExpr

baseDecl :: Parser IRI
baseDecl = string "BASE" >> iri

prefixDecl :: Parser PrefixMapping
prefixDecl = do
  string "PREFIX" >> spaces
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
  isNot <- isJust <$> optionMaybe (string "NOT" <* spaces)
  shape <- shapeAtom
  return $ if isNot then ShapeNot Nothing shape else shape

shapeAtom :: Parser ShapeExpr
shapeAtom =
    withOpt nodeConstraint shapeOrRef (ShapeAnd Nothing) <|>
    shapeOrRef <|>
    between (char '(') (char ')') parseShapeExpr <|>
    (empty <$ (char '.' >> spaces))

shapeOrRef :: Parser ShapeExpr
shapeOrRef = shapeDefinition <|>
             ShapeRef <$> (char '@' *> shapeLabel)

shapeDefinition :: Parser ShapeExpr
shapeDefinition = do
  eoc <- many extraOrClosed
  expr <- between (char '{' >> spaces) (char '}' >> spaces) (optionMaybe tripleExpr)
  -- annotation
  let closed = not $ null (rights eoc)
  let extras = mconcat $ lefts eoc

  return $ Shape Nothing Nothing (Just closed) (Just extras) expr Nothing Nothing

extraOrClosed :: Parser (Either [IRI] Bool)
extraOrClosed = (Right True <$ (string "CLOSED" >> spaces)) <|>
                Left <$> (string "EXTRA" >> spaces >> many1 iri)

inlineShapeExpr :: Parser ShapeExpr
inlineShapeExpr = inlineShapeOr

inlineShapeOr :: Parser ShapeExpr
inlineShapeOr = compositeShape inlineShapeAnd "OR" (ShapeOr Nothing)

inlineShapeAnd :: Parser ShapeExpr
inlineShapeAnd = compositeShape inlineShapeNot "AND" (ShapeAnd Nothing)

inlineShapeNot :: Parser ShapeExpr
inlineShapeNot = do
  isNot <- isJust <$> optionMaybe (string "NOT" >> spaces)
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
  expr <- between (char '{' >> spaces) (char '}' >> spaces) (optionMaybe tripleExpr)
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
compositeShape leaf keyword constructor = do
  sh <- leaf
  shs <- many $ string keyword >> spaces >> leaf
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
innerTripleExpr = multiElementGroup <|> multiElementOneOf

multiElementOneOf :: Parser TripleExpr
multiElementOneOf = do
  left <- groupTripleExpr
  rights <- many (char '|' >> groupTripleExpr)
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
  char '{'
  min <- read <$> many1 digit
  max <- optionMaybe $ do
    char ','
    (Star <$ char '*') <|> (IntMax . read <$> many1 digit)
  char '}'
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

traceState :: Parser ()
traceState = do
  (State s _ _) <- getParserState
  traceShowM s
