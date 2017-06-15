{-# LANGUAGE OverloadedStrings #-}

module Shexkell.Semantic.Datatype where

import Data.RDF (Node(..), LValue(..))

import Data.Text

data Datatype =
    XsdInteger
  | XsdDecimal
  | XsdFloat
  | XsdDouble
  | XsdString
  | XsdBoolean
  | XsdDateTime
  | XsdByte
  | Other Text
  deriving (Show)


getType :: Node -> Maybe Datatype
getType (LNode lval) = getLiteralType lval
getType _            = Nothing

getLiteralType :: LValue -> Maybe Datatype
getLiteralType (TypedL _ uriType) = Just $ getUriType uriType
getLiteralType _                  = Nothing

getUriType :: Text -> Datatype
getUriType "http://www.w3.org/2001/XMLSchema#integer"    = XsdInteger
getUriType "http://www.w3.org/2001/XMLSchema#decimal"    = XsdDecimal
getUriType "http://www.w3.org/2001/XMLSchema#float"      = XsdFloat
getUriType "http://www.w3.org/2001/XMLSchema#double"     = XsdDouble
getUriType "http://www.w3.org/2001/XMLSchema#string"     = XsdString
getUriType "http://www.w3.org/2001/XMLSchema#boolean"    = XsdBoolean
getUriType "http://www.w3.org/2001/XMLSchema#datetime"   = XsdDateTime
getUriType "http://www.w3.org/2001/XMLSchema#byte"       = XsdByte
getUriType iri              = Other iri

isNumeric :: Datatype -> Bool
isNumeric XsdInteger = True
isNumeric XsdDecimal = True
isNumeric XsdFloat = True
isNumeric XsdDouble = True
isNumeric XsdByte = True
isNumeric _ = False

derives :: Datatype -> Datatype -> Bool
derives XsdInteger XsdDecimal = True
derives XsdByte XsdInteger = True
derives XsdByte XsdDecimal = True
derives _ _ = False
