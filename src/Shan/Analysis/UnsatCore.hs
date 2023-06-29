module Shan.Analysis.UnsatCore
  ( SmtFormulaTag (..),
    propertiesName,
    initialName,
    segmentName,
    parseUnsatCore,
    pruneTracesViaUnsatCore,
  )
where

import Data.List (groupBy, isInfixOf)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Shan.Analysis.Trace (Index, Trace, projection)
import Shan.Ast.Diagram (Automaton, Name, aname)
import Shan.Util (Parser, symbolS)
import Text.Megaparsec (choice, manyTill, parse, try)
import Text.Printf (printf)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec as Mega

data SmtFormulaTag
  = Properties
  | Initial Name
  | Segment Name Index
  deriving (Eq, Show)

data AutomatonIndice
  = AutomatonIndice Name [Index]
  deriving (Eq, Show)

data Fragment
  = Fragment Name Index Index
  deriving (Eq, Show)

-- data OptimizationStrategy
--   = WithInitial [Formula]
--   | EliminatingSegments[Formula]
--   | None
--   deriving (Eq, Show)

propertiesName :: String
propertiesName = "$properties"

initialName :: Automaton -> String
initialName = printf "%s,initial" . aname

segmentName :: Name -> Index -> String
segmentName = printf "%s,%d"

smtFormulaTagParser :: Parser SmtFormulaTag
smtFormulaTagParser =
  choice
    [ Properties <$ symbolS (pack propertiesName)
    , try initialParser
    , segmentParser
    ]
  where
    initialParser = do
      n <- manyTill letterChar (symbolS ",initial")
      return (Initial (pack n))
    segmentParser = do
      n <- manyTill letterChar (symbolS ",")
      Segment (pack n) <$> decimal

parseFormula :: String -> SmtFormulaTag
parseFormula s = case parse smtFormulaTagParser "" (pack s) of
  Left e -> error ("parse smt formula tag failed: " ++ Mega.errorBundlePretty e)
  Right tag -> tag

parseUnsatCore :: [String] -> [SmtFormulaTag]
parseUnsatCore = fmap parseFormula

filterSegment :: Eq a => [[a]] -> [a] -> [[a]]
filterSegment lists fragment = filter (isInfixOf fragment) lists

-- filterInitial :: Eq a => [[a]] -> [a] -> [[a]]
-- filterInitial lists initial = filter (isPrefixOf initial) lists

pruneTracesViaUnsatCore ::
  [Trace] ->
  [Automaton] ->
  Trace ->
  [String] ->
  [Trace]
pruneTracesViaUnsatCore ts ms t cores =
  filterSegment ts fragment
  where
    formulas = parseUnsatCore cores
    fragments = indicesToFragment <$> groupByAutomaton formulas
    nameMap = toNameMap ms
    bounds = (\f -> fragmentToBound f nameMap t) <$> fragments
    merge (l, r) (l', r') = (min l l', max r r')
    (li, ri) = foldl merge (maxBound, minBound) bounds
    fragment = slice li ri t

toNameMap :: [Automaton] -> Map Name Automaton
toNameMap = foldl (\m a -> M.insert (aname a) a m) M.empty

fragmentToBound ::
  Fragment ->
  Map Name Automaton ->
  Trace ->
  (Index, Index)
fragmentToBound (Fragment n l r) nmap t =
  let m = nmap ! n
      lt = projection t m
      (_, _, lb) = lt !! l
      (_, _, rb) = lt !! r
   in (lb, rb)

slice :: Int -> Int -> [a] -> [a]
slice from end xs = take (end - from + 1) (drop from xs)

formulaToAutomatonIndice :: SmtFormulaTag -> Maybe AutomatonIndice
formulaToAutomatonIndice (Segment n i) = Just (AutomatonIndice n [i])
formulaToAutomatonIndice (Initial _) = Nothing
formulaToAutomatonIndice Properties = Nothing

groupByAutomaton :: [SmtFormulaTag] -> [AutomatonIndice]
groupByAutomaton =
  fmap mergeAll . groupBy sameAutomaton . mapMaybe formulaToAutomatonIndice
  where
    sameAutomaton (AutomatonIndice n1 _) (AutomatonIndice n2 _) = n1 == n2
    merge (AutomatonIndice n1 is1) (AutomatonIndice n2 is2) = AutomatonIndice (selectName n1 n2) (is1 ++ is2)
    selectName n "" = n
    selectName "" n = n
    selectName n1 n2 = if n1 == n2 then n1 else error "impossible"
    mergeAll = foldl merge (AutomatonIndice "" [])

indicesToFragment :: AutomatonIndice -> Fragment
indicesToFragment (AutomatonIndice n is) = Fragment n (minimum is) (maximum is)
