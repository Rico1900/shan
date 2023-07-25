module Shan.Analysis.ParallelVerification
  ( parallelAnalyze,
    parallelAnalyzeLiteratureCase,
    parallelAnalyzeSynthesizedCase
  )
where

import Shan.Analysis.Pretty (printIsdStatistics, printCaseName)
import Shan.Analysis.Trace (Trace, traces)
import Shan.Analysis.Validation (validateDiagrams)
import Shan.Ast.Diagram (Automaton, Bound, Diagrams, Message)
import Control.Concurrent.STM (newTQueueIO, TQueue, readTQueue, atomically, writeTQueue)
import Shan.Analysis.Guided ( analyzeHanGuidedByTrace )
import Shan.Analysis.UnsatCore ( unsatCoreToFragment )
import Control.Concurrent (getNumCapabilities)
import Data.List (isInfixOf)
import Control.Monad (forever, replicateM_)
import Shan.Util (LiteratureCase (name, path, bound))
import Shan.Parser (parseShan)
import Data.Either (partitionEithers)
import Shan.Synthesis.Synthesizer (SynthesizedCase (caseId, diagrams))
import Shan.Synthesis.Synthesizer qualified as Synth
import Control.Concurrent.Async (async)
import Shan.Pretty (blank)

parallelAnalyzeLiteratureCase :: LiteratureCase -> IO ()
parallelAnalyzeLiteratureCase c = do
  printCaseName (name c)
  sdOrAutomaton <- parseShan (path c)
  let diags = partitionEithers sdOrAutomaton
  parallelAnalyze (bound c) diags

parallelAnalyzeSynthesizedCase :: SynthesizedCase -> IO ()
parallelAnalyzeSynthesizedCase c = do
  printCaseName (caseId c)
  let diags = diagrams c
  let b = Synth.bound c
  parallelAnalyze b diags

parallelAnalyze :: Bound -> Diagrams -> IO ()
parallelAnalyze b (sds, han) = do
  let validationRes = validateDiagrams (sds, han)
  case validationRes of
    Left errorMsg -> print errorMsg
    Right _ ->
      let ts = concatMap traces sds
       in do
            printIsdStatistics b sds ts han
            parallelAnalyzeHanGuidedByTraces b han ts

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
          let remaining = if null filtered
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
