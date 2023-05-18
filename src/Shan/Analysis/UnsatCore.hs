module Shan.Analysis.UnsatCore(
  OptimizationStrategy(..),
  propertiesName,
  initialName,
  segmentName
) where

import Shan.Ast.Diagram (Automaton, aname, Name)
import Text.Printf (printf)
import Data.List (isSuffixOf)
import Data.Text (pack, unpack, splitOn)


data Formula
  = Properties
  | Initial Name
  | Segment Name Int
  deriving (Eq, Show)

data OptimizationStrategy
  = WithInitial
  | EliminatingSegments
  | None
  deriving (Eq, Show)

propertiesName :: String
propertiesName = "properties"

initialName :: Automaton -> String
initialName = printf "%s,initial" . aname

segmentName :: Name -> Int -> String
segmentName = printf "%s,%d"

parseUnsatCore :: [String] -> [Formula]
parseUnsatCore = fmap parseFormula

parseFormula :: String -> Formula
parseFormula s
  | s == "properties" = Properties
  | ",initial" `isSuffixOf` s = Initial (pack (take (length s - 8) s))
  | otherwise = if length splits /= 2
                  then error "impossible"
                  else Segment (head splits) (read $ unpack (splits !! 1))
                where
                  splits = splitOn "," (pack s)