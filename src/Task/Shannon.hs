module Task.Shannon (
  runShannonTask
) where

import Shan.UXF.Uxf (parseUxfFolder)


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
  elements <- parseUxfFolder (path adcBugInt)
  let es = mconcat elements
  mapM_ print es