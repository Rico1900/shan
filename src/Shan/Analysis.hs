module Shan.Analysis(
  analyzeCase,
  analyzeCases
) where

import Shan.Util (Case (..))
import Shan.Parser (parseShan)
import Data.Either (partitionEithers)
import Shan.Analysis.Trace (traces, showTrace, Trace)
import Shan.AST.Diagram (Automaton, Bound)

analyzeCases :: [Case] -> IO ()
analyzeCases = mapM_ analyzeCase

analyzeCase :: Case -> IO ()
analyzeCase c = do
  putStrLn (name c)
  diagrams <- parseShan (path c)
  let (sds, automata) = partitionEithers diagrams
  let ts = concatMap traces sds
  analyzeHanGuidedByTraces (bound c) automata ts

analyzeHanGuidedByTraces :: Bound -> [Automaton] -> [Trace] -> IO ()
analyzeHanGuidedByTraces = undefined

analyzeHanGuidedByTrace :: Bound -> [Automaton] -> Trace -> IO ()
analyzeHanGuidedByTrace = undefined