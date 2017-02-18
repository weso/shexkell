module Shexkell.Data.TripleExpr where

import Shexkell.Data.Common (IRI, ObjectValue)

import Control.DeepSeq (NFData, rnf)

data Annotation = Annotation IRI ObjectValue

data Max = Star | IntMax Int

instance Show Max where
  show Star = "*"
  show (IntMax n) = show n

instance NFData Annotation where
  rnf (Annotation iri ov) = rnf iri `seq` rnf ov

instance NFData Max where
  rnf Star = ()
  rnf (IntMax n) = rnf n
