module Task.Shannon (
  runShannonTask
) where

import Shan.Parser (parseShan)


data Case = Case
  { name :: String,
    path :: FilePath
  }

adcBugInt :: Case
adcBugInt = Case {
  name = "ADC-Bug-int",
  path = "./cases/Shannon/ADC-Bug-int"
}

runShannonTask :: IO ()
runShannonTask = do
  diagrams <- parseShan (path adcBugInt)
  mapM_ print diagrams