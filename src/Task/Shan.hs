module Task.Shan
  ( runShanTask1,
    runShanTask2,
    altitudeDisplayInt,
  )
where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import Shan.Analysis.Guided (analyzeCase, analyzeSynthesizedCase)
import Shan.Pretty (banner)
import Shan.Synthesis.Synthesizer (SynthesisConfig (..), SynthesizedCase (caseId), synthesizeCases)
import Shan.Util (Case (..))
import System.FilePath ((</>))

basePath :: FilePath
basePath = "./cases/Shan"

synthesisConfig :: SynthesisConfig
synthesisConfig =
  SynthesisConfig
    { _caseNum = 6,
      _initialSeed = 2023,
      _boundRange = (3, 10),
      _componentRange = (2, 5),
      _nodeRange = (2, 4),
      _edgeRange = (2, 20),
      _initialEdgeRange = (1, 4),
      _variableCountRange = (3, 20),
      _variableCountWithinNodeRange = (1, 3),
      _variableCountWithinGuardRange = (1, 3),
      _variableCountWithinAssignmentRange = (1, 3),
      _propertyCountRange = (0, 3),
      _constantRange = (0.0, 100.0),
      _itemCountRange = (1, 3),
      _loopBoundRange = (1, 5),
      _intCountRange = (0, 3),
      _intBoundRange = (1, 5),
      _priorityRange = (1, 10),
      _maxLayer = 1
    }

defaultBound :: Int
defaultBound = 3

constructCase :: String -> Int -> Case
constructCase n b =
  if b < 0
    then error "invalid bound"
    else
      Case
        { name = n,
          path = basePath </> n,
          bound = b
        }

yield :: String -> Case
yield = flip constructCase defaultBound

adcBugDInt :: String
adcBugDInt = "ADC-Bug-d-int"

adcBugInt :: String
adcBugInt = "ADC-Bug-int"

altitudeDisplay :: String
altitudeDisplay = "altitude-display"

altitudeDisplayInt :: String
altitudeDisplayInt = "altitude-display-int"

carController :: String
carController = "car-controller"

csmaAut :: String
csmaAut = "csma-aut"

fischerAut :: String
fischerAut = "fischer-aut"

hddi :: String
hddi = "hddi"

learningFactory :: String
learningFactory = "learning-factory"

medicalMonitor :: String
medicalMonitor = "medical-monitor"

waterTanks :: String
waterTanks = "water-tanks"

benchCase :: String -> Benchmark
benchCase s = bench s $ nfIO $ analyzeCase $ yield s

benchSynthesizedCase :: SynthesizedCase -> Benchmark
benchSynthesizedCase sc = bench (caseId sc) $ nfIO $ analyzeSynthesizedCase sc

benchmark1 :: [Benchmark]
benchmark1 =
  [ bgroup
      "experiment 1"
      [ benchCase adcBugDInt,
        benchCase adcBugInt,
        benchCase altitudeDisplay,
        benchCase altitudeDisplayInt,
        benchCase carController,
        benchCase csmaAut,
        benchCase fischerAut,
        benchCase hddi,
        benchCase learningFactory,
        benchCase medicalMonitor,
        benchCase waterTanks
      ]
  ]

benchmarkTest1 :: [Benchmark]
benchmarkTest1 =
  [ bgroup
      "experiment 1"
      [ benchCase altitudeDisplayInt
      ]
  ]

benchmark2 :: [Benchmark]
benchmark2 =
  [ bgroup "experiment 2" (benchSynthesizedCase <$> synthesizeCases synthesisConfig)
  ]

benchmarkTest2 :: [Benchmark]
benchmarkTest2 =
  [ bgroup "experiment 2" (benchSynthesizedCase <$> take 1 (synthesizeCases synthesisConfig))
  ]

banner1 :: IO ()
banner1 = banner "|  experiment 1: literature cases  |"

banner2 :: IO ()
banner2 = banner "|  experiment 2: synthesized cases  |"

runShanTask1 :: IO ()
runShanTask1 = do
  banner1
  defaultMain benchmarkTest1

runShanTask2 :: IO ()
runShanTask2 = do
  banner2
  defaultMain benchmarkTest2
