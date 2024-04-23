module Task.Hant
  ( runExperiment1,
    runExperiment2,
    parallelRunExperiment1,
    parallelRunExperiment2,
    runSingle1,
    runSingle2,
    parallelRunSingle1,
    parallelRunSingle2,
    observeExperiment1,
    observeExperiment2,
  )
where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import Hant.Analysis.Guided (analyzeLiteratureCase, analyzeSynthesizedCase)
import Hant.Analysis.ParallelVerification (parallelAnalyzeLiteratureCase, parallelAnalyzeSynthesizedCase)
import Hant.Pretty (banner)
import Hant.Synthesis.Synthesizer (SynthesisConfig (..), SynthesizedCase (caseId), synthesizeCases)
import Hant.Util (LiteratureCase (..))
import System.FilePath ((</>))

basePath :: FilePath
basePath = "./cases/Shan"

defaultBound :: Int
defaultBound = 3

synthesisConfig :: SynthesisConfig
synthesisConfig =
  SynthesisConfig
    { _caseNum = 10,
      _initialSeed = 2023,
      _checkingBound = defaultBound,
      _componentRange = (5, 10), -- key parameter, baseline: (5,10)
      _nodeRange = (4, 10), -- key parameter, baseline: (4,10)
      _edgeRange = (10, 20), -- key parameter, baseline: (10,20)
      _initialEdgeRange = (1, 4),
      _variableCountRange = (4, 8), -- key parameter, baseline: (4,10)
      _variableCountWithinNodeRange = (1, 3),
      _variableCountWithinAssignmentRange = (1, 3),
      _propertyCountRange = (0, 2),
      _constantRange = (0.0, 10.0),
      _itemCountRange = (1, 3),
      _loopBoundRange = (1, 3),
      _intCountRange = (1, 3), -- key parameter, baseline: (1,3)
      _intBoundRange = (1, 3),
      _priorityRange = (1, 10),
      _maxLayer = 2
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

literatureCaseNames :: [String]
literatureCaseNames =
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
  [ bgroup "experiment 1" (benchLiteratureCase <$> literatureCaseNames)
  ]

parallelBenchmark1 :: [Benchmark]
parallelBenchmark1 =
  [ bgroup
      "experiment 1"
      (parallelBenchLiteratureCase <$> literatureCaseNames)
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
      [ parallelBenchLiteratureCase altitudeDisplay
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

observeExperiment1 :: IO ()
observeExperiment1 = do
  mapM_ parallelAnalyzeLiteratureCase (yield <$> literatureCaseNames)

observeExperiment2 :: IO ()
observeExperiment2 = do
  mapM_ parallelAnalyzeSynthesizedCase (synthesizeCases synthesisConfig)

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
