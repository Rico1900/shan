module Shan.Analysis(
  analyzeCase,
  analyzeCases
) where

import Shan.Util (Case (..))
import Shan.Parser (parseShan)
import Data.Either (partitionEithers)
import Shan.Analysis.Trace (traces, showTrace)

analyzeCases :: [Case] -> IO ()
analyzeCases = mapM_ analyzeCase

analyzeCase :: Case -> IO ()
analyzeCase c = do
  putStrLn (name c)
  diagrams <- parseShan (path c)
  let (sds, automata) = partitionEithers diagrams
  -- mapM_ print sds
  let ts = concatMap traces sds
  print ("length: " ++ show (length ts))
  mapM_ (putStrLn . showTrace) ts