module Task.Shan
  ( runExperiment1,
    runExperiment2,
    parallelRunExperiment1,
    parallelRunExperiment2,
    runSingle1,
    runSingle2,
    parallelRunSingle1,
    parallelRunSingle2,
  )
where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import Shan.Analysis.Guided (analyzeLiteratureCase, analyzeSynthesizedCase)
import Shan.Pretty (banner)
import Shan.Synthesis.Synthesizer (SynthesisConfig (..), SynthesizedCase (caseId), synthesizeCases)
import Shan.Util (LiteratureCase (..))
import System.FilePath ((</>))
import Shan.Analysis.ParallelVerification (parallelAnalyzeLiteratureCase, parallelAnalyzeSynthesizedCase)

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
      _componentRange = (5, 10), -- key parameter
      _nodeRange = (10, 20), -- key parameter
      _edgeRange = (15, 30), -- key parameter
      _initialEdgeRange = (1, 4),
      _variableCountRange = (3, 10), -- key parameter
      _variableCountWithinNodeRange = (1, 3),
      _variableCountWithinGuardRange = (1, 3),
      _variableCountWithinAssignmentRange = (1, 3),
      _propertyCountRange = (0, 3),
      _constantRange = (0.0, 100.0),
      _itemCountRange = (2, 4),
      _loopBoundRange = (1, 5),
      _intCountRange = (1, 3), -- key parameter
      _intBoundRange = (1, 3),
      _priorityRange = (1, 10),
      _maxLayer = 3 -- key parameter
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

parallelBenchLiteratureCase :: String -> Benchmark
parallelBenchLiteratureCase s = bench s $ nfIO $ parallelAnalyzeLiteratureCase $ yield s

parallelBenchSynthesizedCase :: SynthesizedCase -> Benchmark
parallelBenchSynthesizedCase sc = bench (caseId sc) $ nfIO $ parallelAnalyzeSynthesizedCase sc

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

parallelBenchmark1 :: [Benchmark]
parallelBenchmark1 =
  [ bgroup
      "experiment 1"
      [ parallelBenchLiteratureCase adcBugDInt,
        parallelBenchLiteratureCase adcBugInt,
        parallelBenchLiteratureCase altitudeDisplay,
        parallelBenchLiteratureCase altitudeDisplayInt,
        parallelBenchLiteratureCase carController,
        parallelBenchLiteratureCase csmaAut,
        parallelBenchLiteratureCase fischerAut,
        parallelBenchLiteratureCase hddi,
        parallelBenchLiteratureCase learningFactory,
        parallelBenchLiteratureCase medicalMonitor,
        parallelBenchLiteratureCase waterTanks
      ]
  ]

benchmarkSingle1 :: [Benchmark]
benchmarkSingle1 =
  [ bgroup
      "altitude display int"
      [ benchLiteratureCase altitudeDisplayInt
      ]
  ]

parallelBenchmarkSingle1 :: [Benchmark]
parallelBenchmarkSingle1 =
  [ bgroup
      "altitude display int, checking in parallel"
      [ parallelBenchLiteratureCase altitudeDisplayInt
      ]
  ]

benchmark2 :: [Benchmark]
benchmark2 =
  [ bgroup "experiment 2" (benchSynthesizedCase <$> synthesizeCases synthesisConfig)
  ]

parallelBenchmark2 :: [Benchmark]
parallelBenchmark2 =
  [ bgroup "experiment 2" (parallelBenchSynthesizedCase <$> synthesizeCases synthesisConfig)
  ]

benchmarkSingle2 :: [Benchmark]
benchmarkSingle2 =
  [ bgroup "single synthesized case" (benchSynthesizedCase <$> take 1 (synthesizeCases synthesisConfig))
  ]

parallelBenchmarkSingle2 :: [Benchmark]
parallelBenchmarkSingle2 =
  [ bgroup "single synthesized case, checking in parallel" (parallelBenchSynthesizedCase <$> take 1 (synthesizeCases synthesisConfig))
  ]

banner1 :: IO ()
banner1 = banner "|  experiment 1: literature cases  |"

banner2 :: IO ()
banner2 = banner "|  experiment 2: synthesized cases  |"

runExperiment1 :: IO ()
runExperiment1 = do
  banner1
  defaultMain benchmark1

runExperiment2 :: IO ()
runExperiment2 = do
  banner2
  defaultMain benchmark2

parallelRunExperiment1 :: IO ()
parallelRunExperiment1 = do
  banner1
  defaultMain parallelBenchmark1

parallelRunExperiment2 :: IO ()
parallelRunExperiment2 = do
  banner2
  defaultMain parallelBenchmark2

singleBanner1 :: IO ()
singleBanner1 = banner "|  single case: altitude display int  |"

singleBanner2 :: IO ()
singleBanner2 = banner "|  single case: synthesized case  |"

runSingle1 :: IO ()
runSingle1 = do
  singleBanner1
  defaultMain benchmarkSingle1

runSingle2 :: IO ()
runSingle2 = do
  singleBanner2
  defaultMain benchmarkSingle2

parallelRunSingle1 :: IO ()
parallelRunSingle1 = do
  singleBanner1
  defaultMain parallelBenchmarkSingle1

parallelRunSingle2 :: IO ()
parallelRunSingle2 = do
  singleBanner2
  defaultMain parallelBenchmarkSingle2
