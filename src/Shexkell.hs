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
  , NodeShapes

  , validate
  , validateIO
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
import Control.Monad.Par (runPar)
import Data.RDF

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.String (fromString)

import System.IO


-- | Perform the validation of an input Shape Map with a given schema and graph
--   being the input parameters IO handles
validateIO ::
     ShexOptions -- ^ Options of the validation process
  -> Handle      -- ^ Path of the Shape Map file
  -> Handle      -- ^ Path of the RDF graph file
  -> Handle      -- ^ Path of the ShEx schema file
  -> IO ShapeMap -- ^ Resulting Shape Map
validateIO ShexOptions{..} mapHandle graphHandle shexHandle = runParIO $ do  
  -- Spawn the parsing in parallel
  graph' <- spawn $ liftIO (readGraph graphFormat graphHandle :: IO (RDF TList))
  shex'  <- spawn $ liftIO $ readShex  shexFormat  shexHandle
  map'   <- spawn $ liftIO $ readMap mapHandle

  -- Wait for all the parsing tasks to finish
  graph <- get graph'
  shex  <- get shex'
  map   <- get map'

  -- Perform the validation and return the results
  return $ validateMap shex graph (mkMap graph shex map)

-- | Perform the validation of an input Shape Map with a given schema and graph
validate ::
     ShexOptions -- ^ Options of the validation process
  -> String      -- ^ String representation of the Shape Map
  -> String      -- ^ String representation of the RDF graph
  -> String      -- ^ String representation of the ShEx schema
  -> ShapeMap    -- ^ Resulting Shape Map
validate ShexOptions{..} mapStr graphStr shexStr = runPar $ do
  -- Spawn the parsing in parallel
  graph' <- spawnP $ parseGraph graphFormat (fromString graphStr)
  shex'  <- spawnP $ parseSchema shexFormat (fromString shexStr)
  map'   <- spawnP $ parseMap mapStr

  -- Wait for all the parsing tasks to finish
  graph <- get graph'
  shex  <- get shex'
  map   <- get map'

  -- Perform the validation and return the results
  return $ validateMap shex graph (mkMap graph shex map)

  where
    parseGraph :: GraphFormat -> T.Text -> RDF TList
    parseGraph TurtleFormat input = case parseString (TurtleParser Nothing Nothing) input of
      Right parsed -> parsed
      Left err -> error $ show err
    
    parseMap :: String -> [NodeShapes]
    parseMap str = case eitherDecode (fromString str) of
      Right parsed -> parsed
      Left err -> error err


---------------------------------------------------------------
-- * Validation options
---------------------------------------------------------------

-- | Options for the validation
data ShexOptions = ShexOptions{
    shexFormat :: ShexFormat
  , graphFormat :: GraphFormat
}

-- | Specifies the format of the ShEx schema to parse
data ShexFormat = JsonFormat | CompactFormat

-- | Specifies the format of the RDF graph to parse
data GraphFormat = TurtleFormat


---------------------------------------------------------------
-- * Parsing --------------------------------------------------
---------------------------------------------------------------

readGraph :: Rdf gr => GraphFormat -> Handle -> IO (RDF gr)
readGraph TurtleFormat handle = do
  graph <- parseString (TurtleParser Nothing Nothing) <$> TIO.hGetContents handle
  case graph of
    Right parsed -> return parsed
    Left err -> error $ show err

readShex :: ShexFormat -> Handle -> IO Schema
readShex format = fmap (parseSchema format) . hGetContents

readMap :: Handle -> IO [NodeShapes]
readMap handle = do
  file <- B.hGetContents handle
  case eitherDecode file of
    Left err -> error err
    Right result -> return result

parseSchema :: ShexFormat -> String -> Schema
parseSchema format str = case fromFormat format str of
  Left err -> error err
  Right shex -> shex
  where
    fromFormat :: ShexFormat -> String -> Either String Schema
    fromFormat JsonFormat str = parseShex str JSONShexParser
    fromFormat CompactFormat str = parseShex str CompactShexParser

--------------------------------------------
-- * Format of the Shape Map file ----------
--------------------------------------------

-- | Specifies the node to be validated and the shapes
data NodeShapes = NodeShapes {
    node :: Node
  , validation :: [ShapeValidation]
} deriving Show

-- | Relation of Shape and expected result for the validation against
--   that shape
data ShapeValidation = ShapeValidation String ValidationResult
  deriving Show

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
