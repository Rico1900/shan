module Shan.Smt.Lib
  ( example,
    unsatCoreExample
  )
where

import Data.SBV (AllSatResult, EqSymbolic ((.==)), OrdSymbolic ((.>=)), SReal, allSat, sFromIntegral, sIntegers, solve, Symbolic, setOption, namedConstraint, sInteger, (.>), (.<), runSMT, constrain)
import Data.SBV.Control ( SMTOption(ProduceUnsatCores), query, CheckSatResult (Unsat, Sat), checkSat, getUnsatCore )
import Data.SBV.Trans.Control (getValue)
import Text.Printf (printf)
import Data.SBV.Control (getModel)

puzzle :: IO AllSatResult
puzzle = allSat $ do
  [dog, cat, mouse] <- sIntegers ["dog", "cat", "mouse"]
  solve
    [ dog .>= 1, -- at least one dog
      cat .>= 1, -- at least one cat
      mouse .>= 1, -- at least one mouse
      dog + cat + mouse .== 100, -- buy precisely 100 animals
      15 `per` dog + 1 `per` cat + 0.25 `per` mouse .== 100 -- spend exactly 100 dollars
    ]
  where
    p `per` q = p * (sFromIntegral q :: SReal)

example :: IO ()
example = do
  allResults <- puzzle
  print allResults

unsatSubFormula :: Symbolic ()
unsatSubFormula = do 
  c <- sInteger "c"
  constrain $ c .> 0
  constrain $ c .< 2

unsatQuery :: Symbolic [String] 
unsatQuery = do
  [a, b] <- sIntegers ["a", "b"]
  setOption $ ProduceUnsatCores True
  unsatSubFormula
  constrain $ a .== b
  query $ do cs <- checkSat
             case cs of
              Unsat -> getUnsatCore
              Sat -> do
                m <- getModel
                return [show m]
              _ -> error "SMT solver error"

unsatCoreExample :: IO ()
unsatCoreExample = do
  core <- runSMT unsatQuery
  print core