module Lib
    ( entry
    ) where

import Task.Shan (runShanTask, altitudeDisplayInt)

entry :: IO ()
entry = runShanTask