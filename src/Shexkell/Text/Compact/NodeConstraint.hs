module Shexkell.Text.Compact.NodeConstraint where

import Shexkell.Text.Compact.Common

import Shexkell.Data.ShEx
import Shexkell.Data.ShapeExpr
import Shexkell.Data.Common

import Text.ParserCombinators.Parsec

nodeConstraint :: Parser ShapeExpr
nodeConstraint =
      literalKind <|>
      nonLiteralKind <|>
      parseDataType <|>
      valueSet <|>
      (many1 xsFacet >>= \facets -> return $
        NodeConstraint Nothing Nothing Nothing facets Nothing)

literalKind :: Parser ShapeExpr
literalKind = do
  string "LITERAL" >> spaces
  facets <- many xsFacet
  return $ NodeConstraint Nothing (Just [LiteralKind]) Nothing facets Nothing

nonLiteralKind :: Parser ShapeExpr
nonLiteralKind = do
  kind <- (IRIKind <$ string "IRI") <|>
          (BNodeKind <$ string "BNODE") <|>
          (NonLiteralKind <$ string "NONLITERAL")
  facets <- map XsStringFacet <$> many stringFacet
  return $ NodeConstraint Nothing (Just [kind]) Nothing facets Nothing

parseDataType :: Parser ShapeExpr
parseDataType = do
  dt <- iri
  spaces
  facets <- many xsFacet
  return $ NodeConstraint Nothing Nothing (Just dt) facets Nothing

valueSet :: Parser ShapeExpr
valueSet = do
  vs <- between (char '[') (char ']') (many valueSetValue)
  facets <- many xsFacet
  return $ NodeConstraint Nothing Nothing Nothing facets (Just vs)

valueSetValue :: Parser ValueSetValue
valueSetValue = iriRange-- <|> literal

iriRange :: Parser ValueSetValue
iriRange = (do
  i <- iri
  p <- optionMaybe (symbol '~' >> many exclusion)
  return $ case p of
    Nothing -> ObjectValue (IRIValue i)
    Just [] -> Stem i
    Just ex -> StemRange (IRIStem i) (Just ex)) <|>
  StemRange Wildcard . Just <$> (symbol '.' >> many1 exclusion)

exclusion :: Parser ValueSetValue
exclusion = Stem <$> (symbol '-' *> iri <* symbol '~')

xsFacet :: Parser XsFacet
xsFacet = try (XsStringFacet <$> stringFacet) <|> (XsNumericFacet <$> numericFacet)

stringFacet :: Parser StringFacet
stringFacet = try stringLengthFacet <|> patternStringFacet

stringLengthFacet :: Parser StringFacet
stringLengthFacet = do
  strLen <- stringLength
  len    <- read <$> many1 digit
  spaces
  return $ LitStringFacet strLen len

stringLength :: Parser String
stringLength = string "LENGTH" <|>
               try (string "MAXLENGTH") <|>
                   string "MINLENGTH"

patternStringFacet :: Parser StringFacet
patternStringFacet = do
  strPat <- string "PATTERN" <* spaces
  pat    <- between (char '"') (char '"') (many $ noneOf "\n")
  return $ PatternStringFacet strPat pat

numericFacet :: Parser NumericFacet
numericFacet = numericRange <*> numericLiteral

numericRange :: Parser (NumericLiteral -> NumericFacet)
numericRange = try (MinInclusive <$> string "MININCLUSIVE") <|>
                   (MinExclusive <$> string "MINEXCLUSIVE") <|>
                   (MaxInclusive <$> string "MAXINCLUSIVE") <|>
                   (MaxExclusive <$> string "MAXEXCLUSIVE")

numericLiteral :: Parser NumericLiteral
numericLiteral = try (NumericInt . read <$> many1 digit) <|>
                     (NumericDouble . read <$> many1 digit) -- TODO
