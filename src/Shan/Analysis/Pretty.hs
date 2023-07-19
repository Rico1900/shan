module Shan.Analysis.Pretty
  ( modelValues,
    printCaseName,
    printIsdStatistics,
  )
where

import Data.SBV.Internals (SMTModel (modelAssocs))
import Shan.Analysis.Trace (Trace)
import Shan.Ast.Diagram (SequenceDiagram, sdname, splitSequenceDiagram, Automaton, nodeCount, edgeCount)
import Text.Printf (printf)

modelValues :: SMTModel -> String
modelValues m =
  unlines (showAssoc <$> assocs)
  where
    assocs = modelAssocs m
    showAssoc (s, v) = printf "%s = %s" (show s) (show v)

printCaseName :: String -> IO ()
printCaseName n = do
  let nameLen = length n
  putStrLn $ replicate (nameLen + 2) '-'
  putStrLn ("|" ++ n ++ "|")
  putStrLn $ replicate (nameLen + 2) '-'

printIsdStatistics :: [SequenceDiagram] -> [Trace] -> [Automaton] -> IO ()
printIsdStatistics sds ts han = do
  printComponentCount han
  printHanNodesAndEdges han
  printIntCount sds
  printTraceCount (length ts)

printComponentCount :: [Automaton] -> IO ()
printComponentCount han = do
  putStrLn $ "component count: " ++ show (length han)

printHanNodesAndEdges :: [Automaton] -> IO ()
printHanNodesAndEdges han = do
  let nodes = sum (nodeCount <$> han)
  let edges = sum (edgeCount <$> han)
  putStrLn $ "han node count: " ++ show nodes
  putStrLn $ "han edge count: " ++ show edges

printIntCount :: [SequenceDiagram] -> IO ()
printIntCount sds = do
  mapM_ printIntCount' sds
  where
    printIntCount' :: SequenceDiagram -> IO ()
    printIntCount' sd = do
      let (_, ints) = splitSequenceDiagram sd
      putStrLn $ printf "interrupt count of %s: %d" (sdname sd) (length ints)

printTraceCount :: Int -> IO ()
printTraceCount n = do
  putStrLn $ "trace count: " ++ show n