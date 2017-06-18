{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Shexkell.Semantic.NodeConstraint
  (
    satisfies2
  ) where

import Shexkell.Data.ShEx
import Shexkell.Data.Common
import Shexkell.Data.ShapeExpr
import Shexkell.Semantic.Datatype

import Data.RDF (Node(..), LValue(TypedL, PlainL, PlainLL))
import Data.String
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.Regex.TDFA

import Debug.Trace


-- | Check if a node satisfies a node constraint
satisfies2 ::
     ShapeExpr -- ^ Node constraint
  -> Node      -- ^ Node
  -> Bool
satisfies2 = flip satisfiesConstraint



class NConstraint a where
  satisfiesConstraint :: Node -> a -> Bool


instance NConstraint ShapeExpr where
  satisfiesConstraint node NodeConstraint{..} =
    optSatisfies (all $ satisfiesConstraint node) nodeKind &&
    optSatisfies (nodeSatisfiesDataType node) dataType  &&
    optSatisfies (any $ satisfiesConstraint node) values &&
    all (satisfiesConstraint node) xsFacets   


----------------------------------------------------------------------
-- * Node kind
----------------------------------------------------------------------

-- | For a node n and constraint value v, nodeSatisfies(n, v) if:
--
--    * v = "iri" and n is an IRI.
--    * v = "bnode" and n is a blank node.
--    * v = "literal" and n is a Literal.
--    * v = "nonliteral" and n is an IRI or blank node.
instance NConstraint NodeKind where
  satisfiesConstraint (UNode _) IRIKind = True
  satisfiesConstraint (BNode _) BNodeKind = True
  satisfiesConstraint (BNodeGen _) BNodeKind = True
  satisfiesConstraint (LNode _) LiteralKind = True

  satisfiesConstraint (UNode _) NonLiteralKind = True
  satisfiesConstraint (BNode _) NonLiteralKind = True
  satisfiesConstraint (BNodeGen _) NonLiteralKind = True

  satisfiesConstraint _ _ = False


----------------------------------------------------------------------
-- * Node datatype
----------------------------------------------------------------------


-- | For a node n and constraint value v, nodeSatisfies(n, v) if n is an Literal
-- with the datatype v and, if v is in the set of
-- <https://www.w3.org/TR/sparql11-query/#operandDataTypes SPARQL operand data types><https://shexspec.github.io/spec/#bib-sparql11-query [sparql11-query]>,
-- an XML schema string with a value of the lexical form of n can be cast to the
-- target type v per XPath 2.0 section 17, Casting[xpath-functions].
nodeSatisfiesDataType :: Node -> IRI -> Bool
nodeSatisfiesDataType (LNode (TypedL _ uri)) iri = uri == fromString iri
nodeSatisfiesDataType (LNode (PlainL _)) "http://www.w3.org/2001/XMLSchema#string" = True
nodeSatisfiesDataType (LNode (PlainLL _ _ )) "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" = True
nodeSatisfiesDataType _  _                       = False


----------------------------------------------------------------------
-- * Node value
----------------------------------------------------------------------

-- | For a node n and constraint value v, nodeSatisfies(n, v) if n matches some valueSetValue vsv in v. A term matches a valueSetValue if:
--
--    * vsv is an objectValue and n = vsv.
--    * vsv is a Stem with stem st and nodeIn(n, st).
--    * vsv is a StemRange with stem st and exclusions excls and nodeIn(n, st) and there is no x in excls such that nodeIn(n, excl).
--    * vsv is a Wildcard with exclusions excls and there is no x in excls such that nodeIn(n, excl).
instance NConstraint ValueSetValue where
  satisfiesConstraint node (ObjectValue oValue) = satisfiesConstraint node oValue
  satisfiesConstraint (UNode uri) (Stem (IRIStem iri))    = uri == fromString iri 
  satisfiesConstraint node (StemRange Wildcard (Just excl)) = not $ any (satisfiesConstraint node) excl
  satisfiesConstraint _ _ = False

instance NConstraint ObjectValue where
  -- IRI
  satisfiesConstraint (UNode uri) (IRIValue iri) = uri == fromString iri

  -- Literals
  satisfiesConstraint (LNode (PlainL value)) (StringValue str) = fromString str == value
  satisfiesConstraint (LNode (TypedL value t)) (DatatypeString value' t') =
    value == fromString value' && t == fromString t'
  satisfiesConstraint (LNode (PlainLL value lang)) (LangString value' lang') =
    value == fromString value' && lang == fromString lang'

  -- Numerics literals
  satisfiesConstraint n@(LNode (TypedL _ _)) (NumericValue value') = maybe False (== value') (nodeVal n)
 
  satisfiesConstraint _ _ = False

----------------------------------------------------------------------
-- * Xs facets
----------------------------------------------------------------------

instance NConstraint XsFacet where
  satisfiesConstraint node (XsStringFacet strFacet) = satisfiesConstraint node strFacet
  satisfiesConstraint node (XsNumericFacet numFacet) = satisfiesConstraint node numFacet
  
instance NConstraint StringFacet where
  satisfiesConstraint node (LitStringFacet "LENGTH" n)    = (== n) `satisfiesLength` node
  satisfiesConstraint node (LitStringFacet "MINLENGTH" n) = (>= n) `satisfiesLength` node
  satisfiesConstraint node (LitStringFacet "MAXLENGTH" n) = (<= n) `satisfiesLength` node

  satisfiesConstraint node (PatternStringFacet _ patt) =
    maybe False (=~ patt) (T.unpack <$> nodeLex node)

  satisfiesConstraint _ _                              = False

instance NConstraint NumericFacet where
  satisfiesConstraint node (MinInclusive _ lit) = compareLiteral (>= lit) node
  satisfiesConstraint node (MinExclusive _ lit) = compareLiteral (> lit) node
  satisfiesConstraint node (MaxInclusive _ lit) = compareLiteral (<= lit) node
  satisfiesConstraint node (MaxExclusive _ lit) = compareLiteral (< lit) node

  -- | TotalDigits constraint
  satisfiesConstraint node@(LNode (TypedL v t)) (TotalDigits _ n) = 
    -- The value matches the type
    isJust (nodeVal node) &&
    -- The type derives decimal
    mkType t `isDerivedOf` decimalType &&
    -- The length constraint is satisfied
    T.length (T.filter (/= '.') $ canonize v) <= n

  -- | FractionDigits constraint
  satisfiesConstraint node@(LNode (TypedL v t)) (FractionDigits _ n) =
    -- The value matches the type
    isJust (nodeVal node) &&
    -- The type derives decimal
    mkType t `isDerivedOf` decimalType && 
    -- The length constraint is satisfied
    n >= (T.length (T.dropWhile (/= '.') (canonize v)) - 1)

  satisfiesConstraint _    _                   = False 


-- | Obtains the lexic value of a node
nodeLex :: Node -> Maybe T.Text
nodeLex (UNode iri)              = Just iri
nodeLex (BNode text)             = Just text
nodeLex (LNode (PlainL text))    = Just text
nodeLex (LNode (PlainLL text _)) = Just text
nodeLex (LNode (TypedL text _))  = Just text
nodeLex _                        = Nothing

-- | Obtains the numeric value of a typed node
nodeVal :: Node -> Maybe NumericLiteral
nodeVal (LNode (TypedL v t)) = getValue (mkType t) (T.unpack v)
nodeVal _ = Nothing

-- | Obtains the length of the lexic value of a node and feeds it to a given
--   predicate. If the length obtaining fails, returns False
satisfiesLength :: (Int -> Bool) -> Node -> Bool
satisfiesLength f = maybe False (f . T.length) . nodeLex

-- | Obtains the numeric value of a node and feeds it to a given predicate.
--   If the node doesn't have a numeric value, returns False 
compareLiteral :: (NumericLiteral -> Bool) -> Node -> Bool
compareLiteral p = maybe False p . nodeVal

-- | Canonizes a numeric value. Removes trailing and leading zeroes
canonize :: T.Text -> T.Text
canonize = removeTrailing . T.dropWhile (== '0') where
  removeTrailing txt
    | T.any (== '.') txt = T.dropWhileEnd (== '0') txt
    | otherwise = txt



-- | Checks if an optional value satisfies a predicate. If the input doesn't
--   have a value, returns True
optSatisfies ::
    (a -> Bool) -- ^ Predicate
 -> Maybe a     -- ^ Input
 -> Bool
optSatisfies = maybe True
