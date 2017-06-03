module Shexkell.Data.TripleExpr where

import Shexkell.Data.Common (IRI, ObjectValue)


data Annotation = Annotation IRI ObjectValue
  deriving (Show, Eq)

data Max = Star | IntMax Int
  deriving (Eq)

instance Show Max where
  show Star = "*"
  show (IntMax n) = show n
