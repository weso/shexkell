module Shexkell.Text.RDF.Literal where

import Shexkell.Data.Common
import Shexkell.Text.Compact.Common
import Shexkell.Text.Compact.Control

import Text.Parsec (Parsec (..))
import Text.ParserCombinators.Parsec

import Numeric

import Data.Maybe (fromMaybe)


tryParse :: Parsec String () a -> String -> Maybe a
tryParse p = either (const Nothing) Just . parse (p <* eof) "RDF Literal"

-----------------------------------
-- * String Literal
-----------------------------------

rdfLiteral :: ParserShex ObjectValue
rdfLiteral = try langString <|> do
  str <- literalValue
  t <- optionMaybe (string "^^" >> iri)
  return $ case t of
    Just dt -> DatatypeString str dt
    Nothing -> StringValue str

langString :: ParserShex ObjectValue
langString = do
  str <- literalValue
  char '@'
  lang <- many1 (alphaNum <|> char '-')
  return $ LangString str lang

literalValue :: ParserShex String
literalValue = try stringLiteralLong1 <|> try stringLiteralLong2 <|> stringTerminal1 <|> stringTerminal2


-----------------------------------
-- * Numeric Literal
-----------------------------------

intLiteral :: Parsec String st Int
intLiteral = do
  sign <- parseSign
  value <- many1 digit
  return $ read (sign ++ value)


doubleLiteral :: Parsec String st Double
doubleLiteral = do
  sign <- parseSign
  double <- try doubleLeft <|> doubleRight
  return $ read (sign ++ double)

doubleLeft :: Parsec String st String
doubleLeft = do
  left <- many1 digit
  dot <- char '.'
  right <- many digit
  exp <- moption parseExponent
  return $ left ++ (dot:right) ++ exp

doubleRight :: Parsec String st String
doubleRight = do
  dot <- optionMaybe $ char '.'
  right <- many1 digit
  exp <- moption parseExponent
  let left = case dot of
        Just _ -> "0"
        Nothing -> ""
  return $ left ++ right ++ exp

parseExponent :: Parsec String st String
parseExponent = do
  e <- char 'e' <|> char 'E'
  sign <- parseSign
  value <- many1 digit
  return (e:(sign ++ value))

parseSign :: Parsec String st String
parseSign = returnSign <$> optionMaybe (char '+' <|> char '-') where
  returnSign Nothing = []
  returnSign (Just '+') = []
  returnSign (Just '-') = "-"



-----------------------------------
-- * Boolean Literal
-----------------------------------

booleanLiteral :: ParserShex ObjectValue
booleanLiteral = do
  bool <- string "true" <|> string "false"
  return $ DatatypeString bool "http://www.w3.org/2001/XMLSchema#boolean"


----------------------------
-- * Terminals
----------------------------

stringTerminal1 :: ParserShex String
stringTerminal1 = 
  char '\'' *>
  many (noneOf ['\'', '\\', '\n', '\r'] <|> echar <|> uchar)
  <* char '\''

stringTerminal2 :: ParserShex String
stringTerminal2 =
  char '"' *>
  many (noneOf ['"', '\\', '\n', '\r'] <|> echar <|> uchar)
  <* char '"'

stringLiteralLong1 :: ParserShex String
stringLiteralLong1 = mconcat <$> do
  string "'''"
  value <- many $ do
    a <- moption (string "''" <|> string "'")
    b <- noneOf ['\\', '\''] <|> echar <|> uchar
    return $ a ++ [b]
  string "'''"
  return value

stringLiteralLong2 :: ParserShex String
stringLiteralLong2 = mconcat <$> do
  string "\"\"\""
  value <- many $ do
    a <- moption (string "\"" <|> string "\"\"")
    b <- noneOf ['\\', '"'] <|> echar <|> uchar
    return $ a ++ [b]
  string "\"\"\""
  return value


--------------------------------
-- * Escaped chars
--------------------------------

escapedChar :: (Char, Char) -> ParserShex Char
escapedChar (expected, result) = result <$ try (string ['\\', expected])

echar :: ParserShex Char
echar = choice $ map escapedChar [
    ('t', '\t')
  , ('b', '\b')
  , ('n', '\n')
  , ('r', '\r')
  , ('f', '\f')
  , ('\\', '\\')
  , ('"', '"')
  , ('\'', '\'')  
  ]

--------------------------------
-- * Unicode Codepoints
--------------------------------

uchar :: ParserShex Char
uchar = try unicodePoint4 <|> unicodePoint8

unicodePoint4 :: ParserShex Char
unicodePoint4 = readUnicode <$> (string "\\u" >> count 4 hexDigit)

unicodePoint8 :: ParserShex Char
unicodePoint8 = readUnicode <$> (string "\\U" >> count 8 hexDigit)

readUnicode :: String -> Char
readUnicode = toEnum . fst . head . readHex


moption :: Monoid m => Parsec String st m -> Parsec String st m
moption = option mempty