module Shan.Smt.Lib
  ( example,
  )
where

import Data.SBV (AllSatResult, EqSymbolic ((.==)), OrdSymbolic ((.>=)), SReal, allSat, sFromIntegral, sIntegers, solve)

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