module Lib
  ( entry,
  )
where

import System.Environment (getArgs, withArgs)
import Task.Shan (runExperiment1, runExperiment2, runSingle1, parallelRunExperiment1, parallelRunExperiment2, parallelRunSingle1, runSingle2, parallelRunSingle2)

entry :: IO ()
entry = do
  args <- getArgs
  if length args == 1
    then case head args of
      "experiment1" -> withArgs (drop 1 args) runExperiment1
      "experiment2" -> withArgs (drop 1 args) runExperiment2
      "single1" -> withArgs (drop 1 args) runSingle1
      "single2" -> withArgs (drop 1 args) runSingle2
      _ -> error "invalid arguments"
  else if length args == 2
    then case head args of
      "parallel" -> case args !! 1 of
        "experiment1" -> withArgs (drop 2 args) parallelRunExperiment1
        "experiment2" -> withArgs (drop 2 args) parallelRunExperiment2
        "single1" -> withArgs (drop 2 args) parallelRunSingle1
        "single2" -> withArgs (drop 2 args) parallelRunSingle2
        _ -> error "invalid arguments"
      _ -> error "invalid arguments"
  else error "invalid arguments"