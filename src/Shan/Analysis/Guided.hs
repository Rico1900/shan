module Shan.Analysis.Guided(
  analyzeCase,
  analyzeCases
) where

import Shan.Util (Case (..))
import Shan.Parser (parseShan)
import Data.Either (partitionEithers)
import Shan.Analysis.Trace (traces, showTrace, Trace)
import Shan.Ast.Diagram (Automaton, Bound)
import Shan.Analysis.Validation (validateDiagrams)

analyzeCases :: [Case] -> IO ()
analyzeCases = mapM_ analyzeCase

analyzeCase :: Case -> IO ()
analyzeCase c = do
  putStrLn (name c)
  diagrams <- parseShan (path c)
  let (sds, automata) = partitionEithers diagrams
  let validationRes = validateDiagrams (sds, automata)
  case validationRes of 
    Letf io -> io
    Right _ -> let ts = concatMap traces sds
                in analyzeHanGuidedByTraces (bound c) automata ts

analyzeHanGuidedByTraces :: Bound -> [Automaton] -> [Trace] -> IO ()
analyzeHanGuidedByTraces = undefined

analyzeHanGuidedByTrace :: Bound -> [Automaton] -> Trace -> IO ()
analyzeHanGuidedByTrace = undefined

