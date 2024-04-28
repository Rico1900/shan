module Hant.Analysis.ParallelVerification
  ( parallelAnalyze,
    parallelAnalyzeLiteratureCase,
    parallelAnalyzeSynthesizedCase,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Monad (forever, replicateM_)
import Data.Either (partitionEithers)
import Data.List (isInfixOf)
import Hant.Analysis.Guided (analyzeHanGuidedByTrace)
import Hant.Analysis.Pretty (printCaseName, printIsdStatistics)
import Hant.Analysis.Trace (Trace, traces)
import Hant.Analysis.UnsatCore (unsatCoreToFragment)
import Hant.Analysis.Validation (validateDiagrams)
import Hant.Ast.Diagram (Automaton, Bound, Diagrams, Message)
import Hant.Parser (parseShan)
import Hant.Pretty (blank)
import Hant.Synthesis.Synthesizer (SynthesizedCase (caseId, diagrams))
import Hant.Synthesis.Synthesizer qualified as Synth
import Hant.Util (LiteratureCase (bound, name, path))
import Text.Printf (printf)
import Data.Time (getCurrentTime, diffUTCTime)

parallelAnalyzeLiteratureCase :: LiteratureCase -> IO ()
parallelAnalyzeLiteratureCase c = do
  startTime <- getCurrentTime
  printCaseName (name c)
  sdOrAutomaton <- parseShan (path c)
  let diags = partitionEithers sdOrAutomaton
  parallelAnalyze (bound c) diags
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "Time consumption: " ++ show diff

parallelAnalyzeSynthesizedCase :: SynthesizedCase -> IO ()
parallelAnalyzeSynthesizedCase c = do
  startTime <- getCurrentTime
  printCaseName (caseId c)
  let diags = diagrams c
  let b = Synth.bound c
  parallelAnalyze b diags
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "Time consumption: " ++ show diff

parallelAnalyze :: Bound -> Diagrams -> IO ()
parallelAnalyze b (sds, han) = do
  let validationRes = validateDiagrams (sds, han)
  case validationRes of
    [] ->
      let ts = concatMap traces sds
       in do
            printIsdStatistics b sds ts han
            parallelAnalyzeHanGuidedByTraces b han ts
    errorMsg -> print errorMsg

parallelAnalyzeHanGuidedByTraces ::
  Bound ->
  [Automaton] ->
  [Trace] ->
  IO ()
parallelAnalyzeHanGuidedByTraces b han ts = do
  taskQueue <- newTQueueIO
  checkResultQueue <- newTQueueIO
  capabilityCount <- getNumCapabilities
  replicateM_ capabilityCount $ async $ worker b han taskQueue checkResultQueue
  initialize ts taskQueue checkResultQueue

pruner ::
  Int ->
  [Trace] ->
  TQueue Trace ->
  TQueue (Either [Message] String) ->
  IO ()
pruner processingTasks tasks taskQueue checkResultQueue = do
  if processingTasks == 0
    then do
      putStrLn "verified"
      blank
    else do
      checkResult <- atomically $ readTQueue checkResultQueue
      case checkResult of
        Left fragment -> do
          let filtered = filter (not . isInfixOf fragment) tasks
          let pruned = length tasks - length filtered
          let prompt = if pruned == 0
                        then "complete 1 task"
                        else printf "complete 1 task, prune %d tasks" pruned
          putStrLn prompt
          let remaining =
                if null filtered
                  then processingTasks - 1
                  else processingTasks
          mapM_ (atomically . writeTQueue taskQueue) (take 1 filtered)
          pruner remaining (drop 1 filtered) taskQueue checkResultQueue
        Right counterExample -> do
          putStrLn "Counter Example: "
          putStrLn counterExample
          blank

initialize ::
  [Trace] ->
  TQueue Trace ->
  TQueue (Either [Message] String) ->
  IO ()
initialize tasks taskQueue checkResultQueue = do
  capabilityCount <- getNumCapabilities
  let initialTasks = take capabilityCount tasks
  let initialTaskCount = length initialTasks
  mapM_ (atomically . writeTQueue taskQueue) initialTasks
  pruner initialTaskCount (drop initialTaskCount tasks) taskQueue checkResultQueue

worker ::
  Bound ->
  [Automaton] ->
  TQueue Trace ->
  TQueue (Either [Message] String) ->
  IO ()
worker b han taskQueue checkResultQueue = forever $ do
  trace <- atomically $ readTQueue taskQueue
  res <- analyzeHanGuidedByTrace b han trace
  case res of
    Left unsatCore -> do
      let fragment = unsatCoreToFragment trace unsatCore
      atomically $ writeTQueue checkResultQueue (Left fragment)
    Right counterExample -> do
      atomically $ writeTQueue checkResultQueue (Right counterExample)