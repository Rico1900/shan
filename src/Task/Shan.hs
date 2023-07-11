module Task.Shan
  ( runShanTask1,
    runShanTask2,
    altitudeDisplayInt,
  )
where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import Shan.Analysis.Guided (analyzeLiteratureCase, analyzeSynthesizedCase)
import Shan.Pretty (banner)
import Shan.Synthesis.Synthesizer (SynthesisConfig (..), SynthesizedCase (caseId), synthesizeCases)
import Shan.Util (LiteratureCase (..))
import System.FilePath ((</>))

basePath :: FilePath
basePath = "./cases/Shan"

defaultBound :: Int
defaultBound = 3

synthesisConfig :: SynthesisConfig
synthesisConfig =
  SynthesisConfig
    { _caseNum = 6,
      _initialSeed = 2023,
      _checkingBound = defaultBound,
      _componentRange = (2, 5),
      _nodeRange = (2, 4),
      _edgeRange = (2, 10),
      _initialEdgeRange = (1, 4),
      _variableCountRange = (3, 10),
      _variableCountWithinNodeRange = (1, 3),
      _variableCountWithinGuardRange = (1, 3),
      _variableCountWithinAssignmentRange = (1, 3),
      _propertyCountRange = (0, 3),
      _constantRange = (0.0, 100.0),
      _itemCountRange = (2, 4),
      _loopBoundRange = (1, 3),
      _intCountRange = (0, 4),
      _intBoundRange = (1, 3),
      _priorityRange = (1, 10),
      _maxLayer = 1
    }

constructCase :: String -> Int -> LiteratureCase
constructCase n b =
  if b < 0
    then error "invalid bound"
    else
      LiteratureCase
        { name = n,
          path = basePath </> n,
          bound = b
        }

yield :: String -> LiteratureCase
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

benchLiteratureCase :: String -> Benchmark
benchLiteratureCase s = bench s $ nfIO $ analyzeLiteratureCase $ yield s

benchSynthesizedCase :: SynthesizedCase -> Benchmark
benchSynthesizedCase sc = bench (caseId sc) $ nfIO $ analyzeSynthesizedCase sc

benchmark1 :: [Benchmark]
benchmark1 =
  [ bgroup
      "experiment 1"
      [ benchLiteratureCase adcBugDInt,
        benchLiteratureCase adcBugInt,
        benchLiteratureCase altitudeDisplay,
        benchLiteratureCase altitudeDisplayInt,
        benchLiteratureCase carController,
        benchLiteratureCase csmaAut,
        benchLiteratureCase fischerAut,
        benchLiteratureCase hddi,
        benchLiteratureCase learningFactory,
        benchLiteratureCase medicalMonitor,
        benchLiteratureCase waterTanks
      ]
  ]

benchmarkTest1 :: [Benchmark]
benchmarkTest1 =
  [ bgroup
      "experiment 1"
      [ benchLiteratureCase altitudeDisplayInt
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
