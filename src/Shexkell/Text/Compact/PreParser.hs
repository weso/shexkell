module Shexkell.Text.Compact.PreParser (preparse) where

import Text.Parsec

import Control.Monad (void)


preparse :: Parsec String u String
preparse = mconcat <$> many (noneOf "#") `sepBy` parseComment <* eof

parseComment :: Parsec String u String
parseComment = char '#' >> manyTill anyChar (void (many1 newline) <|> eof)