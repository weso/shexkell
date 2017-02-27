module Shexkell.Data.TripleExpr where

import Shexkell.Data.Common (IRI, ObjectValue)


data Annotation = Annotation IRI ObjectValue
  deriving Show

data Max = Star | IntMax Int

instance Show Max where
  show Star = "*"
  show (IntMax n) = show n
