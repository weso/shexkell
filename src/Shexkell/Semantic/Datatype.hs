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


getType :: Node -> Maybe Datatype
getType (LNode lval) = getLiteralType lval
getType _            = Nothing

getLiteralType :: LValue -> Maybe Datatype
getLiteralType (TypedL _ uriType) = getUriType uriType
getLiteralType _                  = Nothing

getUriType :: Text -> Maybe Datatype
getUriType "xsd:integer"  = Just XsdInteger
getUriType "xsd:decimal"  = Just XsdDecimal
getUriType "xsd:float"    = Just XsdFloat
getUriType "xsd:double"   = Just XsdDouble
getUriType "xsd:string"   = Just XsdString
getUriType "xsd:boolean"  = Just XsdBoolean
getUriType "xsd:datetime" = Just XsdDateTime
getUriType _              = Nothing
