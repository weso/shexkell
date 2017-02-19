{-# LANGUAGE RecordWildCards #-}

module Shexkell.Semantic.NodeConstraint
  (
    satisfies2
  , nodeSatisfiesKind
  , nodeSatisfiesDataType
  , nodeSatisfiesValue
  ) where

import Shexkell.Data.ShEx
import Shexkell.Data.Common
import Shexkell.Data.ShapeExpr
import Shexkell.Semantic.Datatype

import Data.RDF (Node(..), LValue(TypedL, PlainL, PlainLL))
import Data.String
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (length)
import Data.Text.Read (decimal, double, Reader)
import Text.Regex.XMLSchema.Generic (match)

-- | Check if a node satisfies a node constraint
satisfies2 ::
     ShapeExpr -- ^ Node constraint
  -> Node      -- ^ Node
  -> Bool
satisfies2 NodeConstraint{..} node =
  optSatisfies (nodeSatisfiesKinds node) nodeKind &&
  optSatisfies (nodeSatisfiesDataType node) dataType  &&
  optSatisfies (nodeSatisfiesValues node) values &&
  nodeSatisfiesFacets node xsFacets


----------------------------------------------------------------------
-- * Node kind
----------------------------------------------------------------------


-- | For a node n and constraint value v, nodeSatisfies(n, v) if:
--
--    * v = "iri" and n is an IRI.
--    * v = "bnode" and n is a blank node.
--    * v = "literal" and n is a Literal.
--    * v = "nonliteral" and n is an IRI or blank node.
nodeSatisfiesKind :: Node -> NodeKind -> Bool
nodeSatisfiesKind (UNode _) IRIKind = True
nodeSatisfiesKind (BNode _) BNodeKind = True
nodeSatisfiesKind (LNode _) LiteralKind = True

nodeSatisfiesKind (UNode _) NonLiteralKind = True
nodeSatisfiesKind (BNode _) NonLiteralKind = True

nodeSatisfiesKind _ _ = False

nodeSatisfiesKinds :: Node -> [NodeKind] -> Bool
nodeSatisfiesKinds = all . nodeSatisfiesKind


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
nodeSatisfiesValue :: Node -> ValueSetValue -> Bool
nodeSatisfiesValue node (ObjectValue oValue) = nodeSatisfiesOValue node oValue
nodeSatisfiesValue (UNode uri) (Stem iri)    = uri == fromString iri

nodeSatisfiesOValue :: Node -> ObjectValue -> Bool
nodeSatisfiesOValue (UNode uri) (IRIValue iri) = uri == fromString iri

nodeSatisfiesValues :: Node -> [ValueSetValue] -> Bool
nodeSatisfiesValues = any . nodeSatisfiesValue


----------------------------------------------------------------------
-- * Xs facets
----------------------------------------------------------------------

nodeSatisfiesFacets :: Node -> [XsFacet] -> Bool
nodeSatisfiesFacets = all . nodeSatisfiesFacet

nodeSatisfiesFacet :: Node -> XsFacet -> Bool
nodeSatisfiesFacet node (XsStringFacet strFacet) = nodeSatisfiesStringFacet node strFacet
nodeSatisfiesFacet node (XsNumericFacet numFacet) = nodeSatisfiesNumericFacet node numFacet

nodeSatisfiesStringFacet :: Node -> StringFacet -> Bool
nodeSatisfiesStringFacet node (LitStringFacet "length" n)    = (== n) `satisfiesLength` node
nodeSatisfiesStringFacet node (LitStringFacet "minlength" n) = (>= n) `satisfiesLength` node
nodeSatisfiesStringFacet node (LitStringFacet "maxlength" n) = (<= n) `satisfiesLength` node

nodeSatisfiesStringFacet node (PatternStringFacet _ patt) =
  maybe False (`match` fromString patt) (nodeLex node)

nodeSatisfiesStringFacet _    _                           = False

nodeLex :: Node -> Maybe Text
nodeLex (UNode iri)              = Just iri
nodeLex (BNode text)             = Just text
nodeLex (LNode (PlainL text))    = Just text
nodeLex (LNode (PlainLL text _)) = Just text
nodeLex (LNode (TypedL text _))  = Just text
nodeLex _                        = Nothing

satisfiesLength :: (Int -> Bool) -> Node -> Bool
satisfiesLength f node = maybe False (f . T.length) (nodeLex node)

nodeSatisfiesNumericFacet :: Node -> NumericFacet -> Bool
nodeSatisfiesNumericFacet node (MinInclusive _ lit) = compareLiteral (<= lit) node
nodeSatisfiesNumericFacet node (MinExclusive _ lit) = compareLiteral (< lit) node
nodeSatisfiesNumericFacet node (MaxInclusive _ lit) = compareLiteral (>= lit) node
nodeSatisfiesNumericFacet node (MaxExclusive _ lit) = compareLiteral (> lit) node
nodeSatisfiesNumericFacet (LNode (TypedL v _)) (TotalDigits _ n) = T.length v == n
nodeSatisfiesNumericFacet _ (FractionDigits _ _)   = False -- TODO
nodeSatisfiesNumericFacet _    _                   = False


numericLiteral :: Node -> Maybe NumericLiteral
numericLiteral (LNode (TypedL value t)) = getUriType t >>= toLiteral where
  toLiteral XsdInteger = NumericInt <$> readTextWith decimal value
  toLiteral XsdDouble  = NumericDouble <$> readTextWith double value
  toLiteral XsdDecimal = NumericDecimal <$> readTextWith double value
  toLiteral _          = Nothing

readTextWith :: Reader a -> Text -> Maybe a
readTextWith reader text = case reader text of
  Left _ -> Nothing
  Right (res, _) -> Just res

compareLiteral :: (NumericLiteral -> Bool) -> Node -> Bool
compareLiteral p = maybe False p . numericLiteral




-- | Checks if an optional value satisfies a predicate. If the input doesn't
--   have a value, returns True
optSatisfies ::
    (a -> Bool) -- ^ Predicate
 -> Maybe a     -- ^ Input
 -> Bool
optSatisfies = maybe True
