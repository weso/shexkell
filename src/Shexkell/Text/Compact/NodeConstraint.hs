module Shexkell.Text.Compact.NodeConstraint where

import Shexkell.Text.Compact.Control
import Shexkell.Text.Compact.Common
import Shexkell.Text.RDF.Literal

import Shexkell.Data.ShEx
import Shexkell.Data.ShapeExpr
import Shexkell.Data.Common

import Text.ParserCombinators.Parsec
import Data.Maybe (fromMaybe)
import Numeric

import Debug.Trace


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
valueSetValue = iriRange <|> literalRange

iriRange :: ParserShex ValueSetValue
-- iriRange = (do
--   i <- iri
--   p <- optionMaybe (symbol '~' >> many iriExclusion)
--   return $ case p of
--     Nothing -> ObjectValue (IRIValue i)
--     Just [] -> Stem i
--     Just ex -> StemRange (IRIStem i) (Just ex)) <|>
--                StemRange Wildcard . Just <$> (symbol '.' >> many1 iriExclusion)

-- literalRange :: ParserShex ValueSetValue

iriRange = range iri iri IRIValue IRIStem (Stem . IRIStem)

literalRange :: ParserShex ValueSetValue
literalRange = range literal literal id LiteralStem (Stem . LiteralStem)


range :: 
     ParserShex a
  -> ParserShex b
  -> (a -> ObjectValue)
  -> (a -> StemValue)
  -> (b -> ValueSetValue)
  -> ParserShex ValueSetValue
range pVal pExc mkObj mkStem mkExc = do
  val <- pVal
  r <- optionMaybe (symbol '~' >> many (exclusion pExc))
  return $ case r of
    Nothing -> ObjectValue (mkObj val)
    Just [] -> Stem $ mkStem val 
    Just ex -> StemRange (mkStem val) (Just $ map mkExc ex)


literal :: ParserShex ObjectValue
literal = rdfLiteral <|> (NumericValue <$> numericLiteral) <|> booleanLiteral


exclusion :: ParserShex a -> ParserShex a
exclusion p = {-Stem <$>-}symbol '-' *> p <* symbol '~'

iriExclusion :: ParserShex ValueSetValue
iriExclusion = Stem . IRIStem <$> exclusion iri

xsFacet :: ParserShex XsFacet
xsFacet = try (XsStringFacet <$> stringFacet) <|> (XsNumericFacet <$> numericFacet)

stringFacet :: ParserShex StringFacet
stringFacet = try stringLengthFacet <|> patternStringFacet

stringLengthFacet :: ParserShex StringFacet
stringLengthFacet = do
  strLen <- stringLength
  len    <- read <$> many1 digit <* skippeables
  return $ LitStringFacet strLen len

stringLength :: ParserShex String
stringLength = keyword "LENGTH" <|>
               try (keyword "MAXLENGTH") <|>
                   keyword "MINLENGTH"

patternStringFacet :: ParserShex StringFacet
patternStringFacet = do
  strPat <- keyword "PATTERN"
  pat    <- char '\"' >> manyTill (noneOf "\"") (char '\"')
  skippeables
  return $ PatternStringFacet strPat pat

numericFacet :: ParserShex NumericFacet
numericFacet = (numericRange <*> numericLiteral) <|>
               (numericLength <*> (read <$> many1 digit <* skippeables))

numericRange :: ParserShex (NumericLiteral -> NumericFacet)
numericRange = try (MinInclusive <$> keyword "MININCLUSIVE") <|>
                   (MinExclusive <$> keyword "MINEXCLUSIVE") <|>
                   (MaxInclusive <$> keyword "MAXINCLUSIVE") <|>
                   (MaxExclusive <$> keyword "MAXEXCLUSIVE")

numericLiteral :: ParserShex NumericLiteral
numericLiteral = try (NumericDouble <$> doubleLiteral <* skippeables)  <|>
                     (NumericInt . read <$> many1 digit <* skippeables)

numericLength :: ParserShex (Int -> NumericFacet)
numericLength = (TotalDigits    <$> keyword "TOTALDIGITS") <|>
                (FractionDigits <$> keyword "FRACTIONDIGITS")

