module Shexkell.Text.Compact.NodeConstraint where

import Shexkell.Text.Compact.Control
import Shexkell.Text.Compact.Common

import Shexkell.Data.ShEx
import Shexkell.Data.ShapeExpr
import Shexkell.Data.Common

import Text.ParserCombinators.Parsec

nodeConstraint :: ParserShex ShapeExpr
nodeConstraint =
      literalKind <|>
      nonLiteralKind <|>
      parseDataType <|>
      valueSet <|>
      (many1 xsFacet >>= \facets -> return $
        NodeConstraint Nothing Nothing Nothing facets Nothing)

literalKind :: ParserShex ShapeExpr
literalKind = do
  keyword "LITERAL"
  facets <- many xsFacet
  return $ NodeConstraint Nothing (Just [LiteralKind]) Nothing facets Nothing

nonLiteralKind :: ParserShex ShapeExpr
nonLiteralKind = do
  kind <- (IRIKind <$ keyword "IRI") <|>
          (BNodeKind <$ keyword "BNODE") <|>
          (NonLiteralKind <$ keyword "NONLITERAL")
  facets <- map XsStringFacet <$> many stringFacet
  return $ NodeConstraint Nothing (Just [kind]) Nothing facets Nothing

parseDataType :: ParserShex ShapeExpr
parseDataType = do
  dt <- iri
  facets <- many xsFacet
  return $ NodeConstraint Nothing Nothing (Just dt) facets Nothing

valueSet :: ParserShex ShapeExpr
valueSet = do
  vs <- between (symbol '[') (symbol ']') (many valueSetValue)
  facets <- many xsFacet
  return $ NodeConstraint Nothing Nothing Nothing facets (Just vs)

valueSetValue :: ParserShex ValueSetValue
valueSetValue = iriRange-- <|> literal

iriRange :: ParserShex ValueSetValue
iriRange = (do
  i <- iri
  p <- optionMaybe (symbol '~' >> many exclusion)
  return $ case p of
    Nothing -> ObjectValue (IRIValue i)
    Just [] -> Stem i
    Just ex -> StemRange (IRIStem i) (Just ex)) <|>
               StemRange Wildcard . Just <$> (symbol '.' >> many1 exclusion)

exclusion :: ParserShex ValueSetValue
exclusion = Stem <$> (symbol '-' *> iri <* symbol '~')

xsFacet :: ParserShex XsFacet
xsFacet = try (XsStringFacet <$> stringFacet) <|> (XsNumericFacet <$> numericFacet)

stringFacet :: ParserShex StringFacet
stringFacet = try stringLengthFacet <|> patternStringFacet

stringLengthFacet :: ParserShex StringFacet
stringLengthFacet = do
  strLen <- stringLength
  len    <- read <$> many1 digit <* spaces
  return $ LitStringFacet strLen len

stringLength :: ParserShex String
stringLength = keyword "LENGTH" <|>
               try (keyword "MAXLENGTH") <|>
                   keyword "MINLENGTH"

patternStringFacet :: ParserShex StringFacet
patternStringFacet = do
  strPat <- keyword "PATTERN"
  pat    <- char '\"' >> manyTill (noneOf "\"") (char '\"')
  spaces
  return $ PatternStringFacet strPat pat

numericFacet :: ParserShex NumericFacet
numericFacet = numericRange <*> numericLiteral

numericRange :: ParserShex (NumericLiteral -> NumericFacet)
numericRange = try (MinInclusive <$> keyword "MININCLUSIVE") <|>
                   (MinExclusive <$> keyword "MINEXCLUSIVE") <|>
                   (MaxInclusive <$> keyword "MAXINCLUSIVE") <|>
                   (MaxExclusive <$> keyword "MAXEXCLUSIVE")

numericLiteral :: ParserShex NumericLiteral
numericLiteral = try (NumericInt . read <$> many1 digit <* spaces) <|>
                     (NumericDouble . read <$> many1 digit <* spaces) -- TODO
