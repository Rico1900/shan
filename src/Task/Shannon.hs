module Task.Shannon (
  runShannonTask
) where

import Shan.UXF.Uxf ()


data Case = Case
  { name :: String,
    path :: FilePath
  }

adcBugInt :: Case
adcBugInt = Case {
  name = "ADC-Bug-int",
  path = "./cases/Shannon/ADC-Bug-int/adc_sd.uxf"
}


runShannonTask :: IO ()
runShannonTask = print ""