module Shan.Analysis.Pretty(
  modelValues,
  printCaseName,
  printIsdStatistics
) where

import Data.SBV.Internals (SMTModel (modelAssocs))
import Text.Printf (printf)
import Shan.Ast.Diagram (SequenceDiagram, splitSequenceDiagram, sdname)
import Shan.Analysis.Trace (Trace)
import Shan.Pretty (separationLine)

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

printIsdStatistics :: [SequenceDiagram] -> [Trace] -> IO ()
printIsdStatistics sds ts = do
  printIntCount sds
  printTraceCount (length ts)

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
  separationLine