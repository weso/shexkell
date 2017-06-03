{-# LANGUAGE RecordWildCards #-}

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
import Data.Text (Text)
import qualified Data.Text as T (length)
import Data.Text.Read (decimal, double, Reader)
import Text.Regex.XMLSchema.Generic (match)


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
  satisfiesConstraint (LNode _) LiteralKind = True

  satisfiesConstraint (UNode _) NonLiteralKind = True
  satisfiesConstraint (BNode _) NonLiteralKind = True

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
  satisfiesConstraint (UNode uri) (Stem iri)    = uri == fromString iri 

instance NConstraint ObjectValue where
  satisfiesConstraint (UNode uri) (IRIValue iri) = uri == fromString iri
  satisfiesConstraint (LNode (PlainL value)) (StringValue str) = fromString str == value
  satisfiesConstraint (LNode (TypedL value t)) (DatatypeString value' t') =
    value == fromString value' && t == fromString t'
  satisfiesConstraint (LNode (PlainLL value lang)) (LangString value' lang') =
    value == fromString value' && lang == fromString lang'
  satisfiesConstraint _ _ = False

----------------------------------------------------------------------
-- * Xs facets
----------------------------------------------------------------------

instance NConstraint XsFacet where
  satisfiesConstraint node (XsStringFacet strFacet) = satisfiesConstraint node strFacet
  satisfiesConstraint node (XsNumericFacet numFacet) = satisfiesConstraint node numFacet
  
instance NConstraint StringFacet where
  satisfiesConstraint node (LitStringFacet "length" n)    = (== n) `satisfiesLength` node
  satisfiesConstraint node (LitStringFacet "minlength" n) = (>= n) `satisfiesLength` node
  satisfiesConstraint node (LitStringFacet "maxlength" n) = (<= n) `satisfiesLength` node

  satisfiesConstraint node (PatternStringFacet _ patt) =
    maybe False (`match` fromString patt) (nodeLex node)

  satisfiesConstraint node _                              = False

instance NConstraint NumericFacet where
  satisfiesConstraint node (MinInclusive _ lit) = compareLiteral (<= lit) node
  satisfiesConstraint node (MinExclusive _ lit) = compareLiteral (< lit) node
  satisfiesConstraint node (MaxInclusive _ lit) = compareLiteral (>= lit) node
  satisfiesConstraint node (MaxExclusive _ lit) = compareLiteral (> lit) node
  satisfiesConstraint (LNode (TypedL v _)) (TotalDigits _ n) = T.length v == n
  satisfiesConstraint _ (FractionDigits _ _)   = False -- TODO
  satisfiesConstraint _    _                   = False 


nodeLex :: Node -> Maybe Text
nodeLex (UNode iri)              = Just iri
nodeLex (BNode text)             = Just text
nodeLex (LNode (PlainL text))    = Just text
nodeLex (LNode (PlainLL text _)) = Just text
nodeLex (LNode (TypedL text _))  = Just text
nodeLex _                        = Nothing

satisfiesLength :: (Int -> Bool) -> Node -> Bool
satisfiesLength f node = maybe False (f . T.length) (nodeLex node)

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
