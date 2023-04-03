{-# LANGUAGE TemplateHaskell #-}

module Shan.UXF.Uxf
  ( UMLType (..),
    Basic (..),
    elementType,
    x,
    y,
    w,
    h,
    content,
    Relation (..),
    element,
    sourceX,
    sourceY,
    targetX,
    targetY,
  )
where

import Control.Lens (makeLenses)
import Xeno.DOM (parse, children)
import Data.ByteString (ByteString)
import 

data UMLType
  = UMLSequenceAllInOne
  | UMLNote
  | UMLSpecialState
  | UMLState
  | RelationType
  deriving (Eq, Show)

parseUMLType :: String -> UMLType
parseUMLType s = case s of
  "UMLSequenceAllInOne" -> UMLSequenceAllInOne
  "UMLNote" -> UMLNote
  "UMLSpecialState" -> UMLSpecialState
  "UMLState" -> UMLState
  "Relation" -> RelationType
  _ -> UMLNote

data Element = BasicE Basic | RelationE Relation
  deriving (Eq, Show)

data Basic = Basic
  { _elementType :: UMLType,
    _x :: Int,
    _y :: Int,
    _w :: Int,
    _h :: Int,
    _content :: String
  }
  deriving (Eq, Show)

data Relation = Relation
  { _element :: Basic,
    _sourceX :: Int,
    _sourceY :: Int,
    _targetX :: Int,
    _targetY :: Int
  }
  deriving (Eq, Show)

makeLenses ''Basic
makeLenses ''Relation

parseUxf :: ByteString -> [Element]
parseUxf bs = case parse bs of 
  Left _ -> []
  Right node -> []

parseUxfFile :: FilePath -> IO [Element]
parseUxfFile p = do 
  