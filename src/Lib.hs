module Lib
  ( entry,
  )
where

import System.Environment ( getArgs )
import Task.Shan (runShanTask1, runShanTask2)

entry :: IO ()
entry = do
  args <- getArgs
  if null args || length args > 1
    then error "invalid arguments"
    else 
      case head args of
        "experiment1" -> runShanTask1
        "experiment2" -> runShanTask2
        _ -> error "invalid arguments"