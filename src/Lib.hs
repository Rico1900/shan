module Lib
  ( entry,
  )
where

import System.Environment (getArgs, withArgs)
import Task.Hant (observeExperiment1, observeExperiment2, parallelRunExperiment1, parallelRunExperiment2, parallelRunSingle1, parallelRunSingle2, runExperiment1, runExperiment2, runSingle1, runSingle2, coverageExperiment1, coverageExperiment2, altitudeDisplayTick, carControllerTick, learningFactoryTick)

entry :: IO ()
entry = do
  args <- getArgs
  if length args >= 2
    then case head args of
      "parallel" -> parallelExperiment (drop 1 args)
      "observe" -> obaserveExperiment (drop 1 args)
      "coverage" -> coverageExperiment (drop 1 args)
      _ -> sequenceExperiment args
    else
      if length args == 1
        then sequenceExperiment args
        else error "invalid arguments"

coverageExperiment :: [String] -> IO ()
coverageExperiment args = do
  case head args of
    "experiment1" -> withArgs (drop 1 args) coverageExperiment1
    "experiment2" -> withArgs (drop 1 args) coverageExperiment2
    _ -> error "invalid arguments"

obaserveExperiment :: [String] -> IO ()
obaserveExperiment args = do
  case head args of
    "experiment1" -> withArgs (drop 1 args) observeExperiment1
    "experiment2" -> withArgs (drop 1 args) observeExperiment2
    "altitude-display" -> altitudeDisplayTick
    "car-controller" -> carControllerTick
    "learning-factory" -> learningFactoryTick
    _ -> error "invalid arguments"

parallelExperiment :: [String] -> IO ()
parallelExperiment args = do
  case head args of
    "experiment1" -> withArgs (drop 1 args) parallelRunExperiment1
    "experiment2" -> withArgs (drop 1 args) parallelRunExperiment2
    "single1" -> withArgs (drop 1 args) parallelRunSingle1
    "single2" -> withArgs (drop 1 args) parallelRunSingle2
    _ -> error "invalid arguments"

sequenceExperiment :: [String] -> IO ()
sequenceExperiment args = do
  case head args of
    "experiment1" -> withArgs (drop 1 args) runExperiment1
    "experiment2" -> withArgs (drop 1 args) runExperiment2
    "single1" -> withArgs (drop 1 args) runSingle1
    "single2" -> withArgs (drop 1 args) runSingle2
    _ -> error "invalid arguments"