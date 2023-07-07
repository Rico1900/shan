module Task.Shan
  ( runShanTask1,
    runShanTask2,
    altitudeDisplayInt,
  )
where

import System.FilePath ((</>))
import Shan.Util (Case (..))
import Shan.Analysis.Guided (analyzeCases, analyzeCase, analyzeSynthesizedCase)
import Shan.Synthesis.Synthesizer (SynthesisConfig (..), synthesizeCases)

basePath :: FilePath
basePath = "./cases/Shan"

synthesisConfig :: SynthesisConfig
synthesisConfig =
  SynthesisConfig
    { _caseNum = 6,
      _initialSeed = 2023,
      _boundRange = (3, 20),
      _componentRange = (2, 10),
      _nodeRange = (2, 20),
      _edgeRange = (2, 40),
      _initialEdgeRange = (1, 4),
      _variableCountRange = (3, 20),
      _variableCountWithinNodeRange = (1, 3),
      _variableCountWithinGuardRange = (1, 3),
      _variableCountWithinAssignmentRange = (1, 3),
      _propertyCountRange = (0, 3),
      _constantRange = (0.0, 100.0),
      _itemCountRange = (1, 5),
      _loopBoundRange = (1, 6),
      _intCountRange = (0, 5),
      _intBoundRange = (1, 10),
      _priorityRange = (1, 10),
      _maxLayer = 2
    }

constructCase :: String -> Int -> Case
constructCase n b =
  if b < 0
    then error "invalid bound"
    else  Case
      { name = n,
        path = basePath </> n,
        bound = b
      }
  
adcBugDInt :: Case
adcBugDInt = constructCase "ADC-Bug-d-int" 3

adcBugInt :: Case
adcBugInt = constructCase "ADC-Bug-int" 3

altitudeDisplay :: Case
altitudeDisplay = constructCase "altitude-display" 3

altitudeDisplayInt :: Case
altitudeDisplayInt = constructCase "altitude-display-int" 5

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

runShanTask1 :: IO ()
runShanTask1 = do
  banner1
  -- runShanTask = analyzeCases benchmark
  analyzeCase altitudeDisplayInt

runShanTask2 :: IO ()
runShanTask2 = do
  banner2
  let synthesizedCases = synthesizeCases synthesisConfig
  print (head synthesizedCases)
  analyzeSynthesizedCase (head synthesizedCases)

banner1 :: IO ()
banner1 = banner "|  experiment 1: literature cases  |"

banner2 :: IO ()
banner2 = banner "|  expriment 2: synthesized cases  |"

banner :: String -> IO ()
banner info = do
  let l = length info
  line l
  putStrLn info
  line l

line :: Int -> IO ()
line n = putStrLn $ replicate n '-'