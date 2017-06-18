{-# LANGUAGE OverloadedStrings #-}

module Shexkell.Semantic.Datatype where

import qualified Data.Text as T

import Data.Maybe
import Data.List

import Shexkell.Text.RDF.Literal
import Shexkell.Data.Common


-----------------------------------------------
-- XSD Datatypes
-----------------------------------------------

-- | Data representation of an XSD Datatype
data UriDatatype = UriDatatype {
    uri :: T.Text                 -- ^ URI of the type
  , parent :: Maybe UriDatatype   -- ^ Type that this one derives from
} deriving (Eq)

builtinDatatypes :: [UriDatatype]
builtinDatatypes = [
    decimalType 
  , integerType 
  , floatType   
  , doubleType  
  , stringType  
  , booleanType 
  , dateTimeType 
  , byteType    
  ]

decimalType :: UriDatatype
decimalType = UriDatatype "http://www.w3.org/2001/XMLSchema#decimal" Nothing

integerType :: UriDatatype
integerType = UriDatatype "http://www.w3.org/2001/XMLSchema#integer" (Just decimalType)

floatType :: UriDatatype
floatType = UriDatatype "http://www.w3.org/2001/XMLSchema#float" Nothing

doubleType :: UriDatatype
doubleType = UriDatatype "http://www.w3.org/2001/XMLSchema#double" Nothing

stringType :: UriDatatype
stringType = UriDatatype "http://www.w3.org/2001/XMLSchema#string" Nothing

booleanType :: UriDatatype
booleanType = UriDatatype "http://www.w3.org/2001/XMLSchema#boolean" Nothing

dateTimeType :: UriDatatype
dateTimeType = UriDatatype "http://www.w3.org/2001/XMLSchema#datetime" Nothing

byteType :: UriDatatype
byteType = UriDatatype "http://www.w3.org/2001/XMLSchema#byte" (Just integerType)


-- | Creates an 'UriDatatype' from its URI
mkType :: T.Text -> UriDatatype
mkType typeName = fromMaybe 
  (UriDatatype typeName Nothing)
  (find ((== typeName) . uri) builtinDatatypes)

-- | Given two datatypes, checks if the first one derives the second one
isDerivedOf :: UriDatatype -> UriDatatype -> Bool
t `isDerivedOf` t' | t == t' = True
(UriDatatype _ Nothing) `isDerivedOf` _ = False
(UriDatatype _ (Just p)) `isDerivedOf` t'@(UriDatatype u' _) = uri p == u' || p `isDerivedOf` t'

-----------------------------------------------
-- * TypedLiteral
-----------------------------------------------

-- | Represents a type whose value can be obtained by its datatype and string
--   representation
class (Eq v, Ord v) => TypedLiteral v where
  getValue :: UriDatatype -> String -> Maybe v

instance TypedLiteral NumericLiteral where
  getValue (UriDatatype "http://www.w3.org/2001/XMLSchema#decimal" _) = fmap NumericDecimal . tryParse doubleLiteral
  getValue (UriDatatype "http://www.w3.org/2001/XMLSchema#double" _)  = fmap NumericDouble  . tryParse doubleLiteral
  getValue (UriDatatype "http://www.w3.org/2001/XMLSchema#integer" _) = fmap NumericInt     . tryParse intLiteral
  getValue (UriDatatype "http://www.w3.org/2001/XMLSchema#byte" _)    = fmap NumericInt     . tryParse intLiteral
  getValue (UriDatatype "http://www.w3.org/2001/XMLSchema#float" _)   = fmap NumericDouble  . tryParse doubleLiteral
  getValue _ = const Nothing

