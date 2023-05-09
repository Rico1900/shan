module Task.Shan
  ( runShanTask,
  )
where

import System.FilePath ((</>))
import Shan.Util (Case (..))
import Shan.Analysis (analyzeCases, analyzeCase)
import Shan.SMT.Lib (example)

basePath :: FilePath
basePath = "./cases/Shan"

constructCase :: String -> Case
constructCase n =
  Case
    { name = n,
      path = basePath </> n
    }

adcBugDInt :: Case
adcBugDInt = constructCase "ADC-Bug-d-int"

adcBugInt :: Case
adcBugInt = constructCase "ADC-Bug-int"

altitudeDisplay :: Case
altitudeDisplay = constructCase "altitude-display"

altitudeDisplayInt :: Case
altitudeDisplayInt = constructCase "altitude-display-int"

carController :: Case
carController = constructCase "car-controller"

csmaAut :: Case
csmaAut = constructCase "csma-aut"

fischerAut :: Case
fischerAut = constructCase "fischer-aut"

hddi :: Case
hddi = constructCase "hddi"

learningFactory :: Case
learningFactory = constructCase "learning-factory"

medicalMonitor :: Case
medicalMonitor = constructCase "medical-monitor"

waterTanks :: Case
waterTanks = constructCase "water-tanks"

benchmark :: [Case]
benchmark = 
  [ adcBugDInt, 
    adcBugInt, 
    altitudeDisplay, 
    altitudeDisplayInt, 
    carController, 
    csmaAut, 
    fischerAut,
    hddi,
    learningFactory,
    medicalMonitor,
    waterTanks
  ]

runShanTask :: IO ()
runShanTask = example
-- runShanTask = analyzeCases benchmark
-- runShanTask = analyzeCase altitudeDisplayInt