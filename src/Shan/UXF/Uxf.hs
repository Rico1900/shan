{-# LANGUAGE TemplateHaskell #-}

module Shan.UXF.Uxf
  ( UMLType (..),
    Basic (..),
    Element (..),
    Relation (..),
    elementType,
    x,
    y,
    w,
    h,
    content,
    element,
    sourceX,
    sourceY,
    targetX,
    targetY,
    parseUxfFile,
    parseUxfFolder,
  )
where

import Control.Lens (makeLenses, to, (^.), (^..))
import Control.Lens.Combinators (folded)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Shan.UXF.HtmlProcessor (processHtmlEntries)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import Xeno.DOM (Content (Text), Node, children, contents, name, parse)

data UMLType
  = UMLSequenceAllInOne
  | UMLNote
  | UMLSpecialState
  | UMLState
  | RelationType
  deriving (Eq, Show)

parseUMLType :: ByteString -> UMLType
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
    _content :: Text
  }
  deriving (Eq, Show)

data Relation = Relation
  { _element :: Basic,
    _sourceX :: Double,
    _sourceY :: Double,
    _targetX :: Double,
    _targetY :: Double
  }
  deriving (Eq, Show)

makeLenses ''Basic
makeLenses ''Relation
makeLenses ''Content

parseUxf :: ByteString -> [Element]
parseUxf bs = case parse bs of
  Left _ -> []
  Right node -> nodeToElement <$> findChildrenByName "element" node

nodeToElement :: Node -> Element
nodeToElement n =
  let b = nodeToBasic n
   in if (b ^. elementType) /= RelationType
        then BasicE b
        else
          let aa =
                maybe
                  (error "no additional attributes")
                  (unpack . textContent)
                  (findChildByName "additional_attributes" n)
              al = splitOn ";" aa
              parseR i = fromMaybe (error "parsing relation failed") . readMaybe $ al !! i
              sx :: Double = parseR 0
              sy :: Double = parseR 1
              tx :: Double = parseR 2
              ty :: Double = parseR 3
           in RelationE $ Relation b sx sy tx ty

nodeToBasic :: Node -> Basic
nodeToBasic n =
  Basic et xi yi wi hi pa
  where
    et = parseUMLType $ maybe "" textContent (findChildByName "id" n)
    coord = fromMaybe (error "no coordinates") (findChildByName "coordinates" n)
    parseFromCoord s =
      fromMaybe (error ("parsing " ++ s ++ " failed"))
        . readMaybe
        . unpack
        . textContent
        . fromMaybe (error ("no " ++ s))
        $ findChildByName (pack s) coord
    xi :: Int = parseFromCoord "x"
    yi :: Int = parseFromCoord "y"
    wi :: Int = parseFromCoord "w"
    hi :: Int = parseFromCoord "h"
    pa = maybe (error "no attributes") (processHtmlEntries . decodeUtf8 . textContent) (findChildByName "panel_attributes" n)

findChildrenByName :: ByteString -> Node -> [Node]
findChildrenByName n parent = filter (\node -> name node == n) (children parent)

findChildByName :: ByteString -> Node -> Maybe Node
findChildByName n parent = find (\node -> name node == n) (children parent)

textContent :: Node -> ByteString
textContent node = mconcat (contents node ^.. folded . to text)
  where
    text c = case c of
      Text t -> t
      _ -> ""

parseUxfFile :: FilePath -> IO [Element]
parseUxfFile p = do
  c <- BS.readFile p
  return $ parseUxf c

parseUxfFolder :: FilePath -> IO [[Element]]
parseUxfFolder p = do
  files <- listDirectory p
  let absFiles = (p </>) <$> files
  traverse parseUxfFile absFiles