module Lib
  ( entry,
  )
where

import System.Environment (getArgs, withArgs)
import Task.Shan (runShanTask1, runShanTask2)

entry :: IO ()
entry = do
  args <- getArgs
  if null args || length args > 1
    then error "invalid arguments"
    else case head args of
      "experiment1" -> withArgs (drop 1 args) runShanTask1
      "experiment2" -> withArgs (drop 1 args) runShanTask2
      _ -> error "invalid arguments"