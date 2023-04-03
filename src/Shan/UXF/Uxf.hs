{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

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
    targetY
  )
where

import Control.Lens (makeLenses)
import Xeno.DOM (parse, children, name, Node)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.Directory (listDirectory)

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
  Right node -> nodeToElement <$> findChildrenByName "element" node

nodeToElement :: Node -> Element
nodeToElement = undefined

nodeToBasic :: Node -> Basic
nodeToBasic n = undefined


parseUxfFile :: FilePath -> IO [Element]
parseUxfFile p = do 
  c <- BS.readFile p
  return $ parseUxf c

parseUxfFolder :: FilePath -> IO [[Element]]
parseUxfFolder p = do
  files <- listDirectory p
  traverse parseUxfFile files

findChildrenByName :: ByteString -> Node -> [Node]
findChildrenByName n parent = filter (\node -> name node == n) (children parent)