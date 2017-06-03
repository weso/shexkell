{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Shexkell (
    Schema(..)
  , ShapeExpr(..)
  , TripleExpr(..)
  , ShapeMap(..)
  , shexId

  , triplMin
  , triplMax


  , SemAct(..)
  , ObjectValue(..)
  , IRI
  , ShapeLabel(..)

  , NodeKind(..)
  , XsFacet(..)
  , StringFacet(..)
  , NumericFacet(..)
  , NumericLiteral(..)
  , ValueSetValue(..)
  , StemValue(..)

  , Annotation(..)
  , Max(..)

  , ShexParser
  , JSONShexParser
  , CompactShexParser

  , validate
  , ShexOptions(..)
  , ShexFormat(..)
  , GraphFormat(..)
) where

import Shexkell.Data.Common
import Shexkell.Data.ShEx
import Shexkell.Data.ShapeExpr
import Shexkell.Data.TripleExpr
import Shexkell.Data.ShapeMap

import Control.DeepSeq
import Shexkell.Control.DeepSeq ()

import Shexkell.Text

import Shexkell.Semantic.Validation


import Control.Monad.IO.Class
import Control.Monad.Par.Class
import Control.Monad.Par.IO
import Data.RDF

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T


-- | Perform the validation of an input Shape Map with a given schema and graph
validate ::
     ShexOptions -- ^ Options of the validation process
  -> FilePath    -- ^ Path of the Shape Map file
  -> FilePath    -- ^ Path of the RDF graph file
  -> FilePath    -- ^ Path of the ShEx schema file
  -> IO ShapeMap -- ^ Resulting Shape Map
validate ShexOptions{..} mapPath graphPath shexPath = runParIO $ do  
  graph' <- spawn $ liftIO (readGraph graphFormat graphPath :: IO (RDF TList))
  shex'  <- spawn $ liftIO $ readShex  shexFormat  shexPath
  map'   <- spawn $ liftIO $ readMap mapPath
  graph <- get graph'
  shex <- get shex'
  map <- get map'
  return $ validateMap shex graph (mkMap graph shex map)


-- | Options for the validation
data ShexOptions = ShexOptions {
    shexFormat :: ShexFormat
  , graphFormat :: GraphFormat
}

-- | Specifies the format of the ShEx schema to parse
data ShexFormat = JsonFormat | CompactFormat

-- | Specifies the format of the RDF graph to parse
data GraphFormat = TurtleFormat


---------------------------------------------------------------
-- * File reading ---------------------------------------------
---------------------------------------------------------------

readGraph :: Rdf gr => GraphFormat -> FilePath -> IO (RDF gr)
readGraph TurtleFormat path = do
  graph <- parseFile (TurtleParser Nothing Nothing) path
  case graph of
    Right parsed -> return parsed
    Left err -> error $ show err

readShex :: ShexFormat -> FilePath -> IO Schema
readShex format path = do
  file <- readFile path
  case fromFormat format file of
    Left err -> error err
    Right shex -> return shex
  where
    fromFormat :: ShexFormat -> String -> Either String Schema
    fromFormat JsonFormat str = parseShex str JSONShexParser
    fromFormat CompactFormat str = parseShex str CompactShexParser

readMap :: FilePath -> IO [NodeShapes]
readMap path = do
  file <- B.readFile path
  case eitherDecode file of
    Left err -> error err
    Right result -> return result


--------------------------------------------
-- * Format of the Shape Map file ----------
--------------------------------------------

-- | Specifies the node to be validated and the shapes
data NodeShapes = NodeShapes {
    node :: Node
  , validation :: [ShapeValidation]
}

-- | Relation of Shape and expected result for the validation against
--   that shape
data ShapeValidation = ShapeValidation String ValidationResult

-- | Specifies the structure of the Shape Map format to create the
--   ShapeMap
instance ShapeMapRef [NodeShapes] NodeShapes ShapeValidation where
  nodes = id

  shapes _ NodeShapes{..} = validation  

  mkNode _ NodeShapes{..} = node

  mkShapeLabel _ (ShapeValidation ('_':(':':bnodeLbl)) _) = BNodeId bnodeLbl
  mkShapeLabel _ (ShapeValidation lbl _) = IRILabel lbl

  result _ (ShapeValidation _ r) = r


--------------------------------------------
-- * JSON parsing / serializing ------------
--------------------------------------------

-- Parsing

instance FromJSON NodeShapes where
  parseJSON = withObject "Node Shapes" $ \ o -> do
    node <- o .: "node"
    validation <- o .: "shapes"
    return NodeShapes{..}

instance FromJSON ShapeValidation where
  parseJSON = withObject "Shape Validation" $ \ o -> do
    shapeLabel <- o .: "shape"
    res        <- o .: "result"
    return $ ShapeValidation shapeLabel res

instance FromJSON ValidationResult where    
  parseJSON (String "positive") = return Positive
  parseJSON (String "negative") = return Negative

instance FromJSON Node where
  parseJSON = withObject "Node" $ \ o -> do
    nType <- (o .: "nodeType") :: Parser T.Text
    v <- o .: "value"
    t <- o .:? "type"
    return $ case nType of
      "blank" -> bnode v
      "unode" -> unode v
      "literal" -> lnode $ case t of
        Just nt -> typedL v nt 
        Nothing -> plainL v

-- Serializing

instance ToJSON ShapeMap where
  toJSON (ShapeMap shmap) = toJSON [object ["node" .= node, "shapes" .= map jsonShape shapes] | (node, shapes) <- M.toList shmap]
    where jsonShape (shape, res) = object ["shape" .= shape, "result" .= res]

instance ToJSON ShapeExpr where
  toJSON = String . T.pack . maybe "" showLabel . shexId

instance ToJSON ValidationResult where
  toJSON Positive = String "positive"
  toJSON Negative = String "negative"

instance ToJSON Node where
  toJSON n@(UNode text) = object ["nodeType" .= nodeType n, "value" .= text]
  toJSON n@(BNode value) = object ["nodeType" .= nodeType n, "value" .= value]
  toJSON n@(LNode (PlainL value)) = object ["nodeType" .= nodeType n, "value" .= value]
  toJSON n@(LNode (TypedL value t)) = object ["nodeType" .= nodeType n, "value" .= value, "type" .= t]


nodeType :: Node -> String
nodeType (UNode _) = "unode"
nodeType (BNode _) = "bnode"
nodeType (LNode _) = "literal"

showLabel :: ShapeLabel -> String
showLabel (IRILabel iri) = iri
showLabel (BNodeId value) = "_:" ++ value


instance NFData ShapeValidation where
  rnf (ShapeValidation shape res) = res `seq` rnf shape

instance NFData NodeShapes where
  rnf NodeShapes{..} = rnf node `seq` rnf validation
