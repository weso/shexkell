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
import Shexkell.Utils.Either


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
import Data.Maybe (isJust)

import System.IO


-- | Perform the validation of an input Shape Map with a given schema and graph
--   being the input parameters IO handles
validateIO ::
     ShexOptions -- ^ Options of the validation process
  -> Handle      -- ^ Path of the Shape Map file
  -> Handle      -- ^ Path of the RDF graph file
  -> Handle      -- ^ Path of the ShEx schema file
  -> IO (Either String ShapeMap) -- ^ Resulting Shape Map
validateIO ShexOptions{..} mapHandle graphHandle shexHandle = runParIO $ do  
  -- Spawn the parsing in parallel
  graph' <- spawnEitherIO $ readGraph graphFormat graphHandle
  shex'  <- spawnEitherIO $ readShex  shexFormat  shexHandle
  map'   <- spawnEitherIO $ readMap mapHandle

  -- Wait for all the parsing tasks to finish
  graph <- get graph'
  shex  <- get shex'
  map   <- get map'

  -- Perform the validation and return the results
  return $ do
    g <- graph
    s <- shex
    m <- map
    return $ validateMap s g (mkMap g s m)

  where 
    spawnEitherIO :: (NFData a, NFData b) => EitherT a IO b -> ParIO (IVar (Either a b))
    spawnEitherIO = spawn . liftIO . runEitherT

-- | Perform the validation of an input Shape Map with a given schema and graph
validate ::
     ShexOptions               -- ^ Options of the validation process
  -> String                    -- ^ String representation of the Shape Map
  -> String                    -- ^ String representation of the RDF graph
  -> String                    -- ^ String representation of the ShEx schema
  -> Either String ShapeMap    -- ^ Resulting Shape Map
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
  return $ do
    g <- graph
    s <- shex
    m <- map
    return $ validateMap s g (mkMap g s m)

  where
    parseGraph :: GraphFormat -> T.Text -> Either String (RDF TList)
    parseGraph TurtleFormat = mapLeft show . parseString (TurtleParser Nothing Nothing) 
    
    parseMap :: String -> Either String [NodeShapes]
    parseMap str = mapLeft show $ eitherDecode $ fromString str


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

readGraph :: GraphFormat -> Handle -> EitherT String IO (RDF TList)
readGraph TurtleFormat handle = do
  contents <- lift $ TIO.hGetContents handle
  mapLeftT show $ hoistEither $ parseString (TurtleParser Nothing Nothing) contents

readShex :: ShexFormat -> Handle -> EitherT String IO Schema
readShex format h = do 
  content <- lift $ hGetContents h
  hoistEither $ parseSchema format content

readMap :: Handle -> EitherT String IO [NodeShapes]
readMap handle = do
  file <- lift $ B.hGetContents handle
  mapLeftT show $ hoistEither $ eitherDecode file

parseSchema :: ShexFormat -> String -> Either String Schema
parseSchema JsonFormat    = flip parseShex JSONShexParser
parseSchema CompactFormat = flip parseShex CompactShexParser


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
  toJSON (ShapeMap shmap) = toJSON [object ["node" .= node, "shapes" .= [jsonShape sh | sh <- shapes, hasLabel sh]] | (node, shapes) <- M.toList shmap]
    where jsonShape (shape, res) = object ["shape" .= shape, "result" .= res]
          hasLabel (shape, _) = isJust $ shexId shape

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
