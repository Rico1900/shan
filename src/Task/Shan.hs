module Task.Shan
  ( runShanTask,
    altitudeDisplayInt,
  )
where

import System.FilePath ((</>))
import Shan.Util (Case (..))
import Shan.Analysis.Guided (analyzeCases, analyzeCase)
import Shan.Smt.Lib (example, unsatCoreExample)

basePath :: FilePath
basePath = "./cases/Shan"

constructCase :: String -> Int -> Case
constructCase n bound =
  if bound < 0
    then error "invalid bound"
    else  Case
      { name = n,
        path = basePath </> n,
        bound = bound
      }
  

adcBugDInt :: Case
adcBugDInt = constructCase "ADC-Bug-d-int" 3

adcBugInt :: Case
adcBugInt = constructCase "ADC-Bug-int" 3

altitudeDisplay :: Case
altitudeDisplay = constructCase "altitude-display" 3

altitudeDisplayInt :: Case
altitudeDisplayInt = constructCase "altitude-display-int" 1

carController :: Case
carController = constructCase "car-controller" 3

csmaAut :: Case
csmaAut = constructCase "csma-aut" 3

fischerAut :: Case
fischerAut = constructCase "fischer-aut" 3

hddi :: Case
hddi = constructCase "hddi" 3

learningFactory :: Case
learningFactory = constructCase "learning-factory" 3

medicalMonitor :: Case
medicalMonitor = constructCase "medical-monitor" 3

waterTanks :: Case
waterTanks = constructCase "water-tanks" 3

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
-- runShanTask = unsatCoreExample
-- runShanTask = analyzeCases benchmark
runShanTask = analyzeCase altitudeDisplayInt