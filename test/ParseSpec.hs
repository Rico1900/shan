{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

module ParseSpec(
  parseSpec
) where

import Shan.Parser (parseShan)
import Test.Syd (Spec, describe, it)
import Task.Shan (altitudeDisplayInt)
import Shan.Util (Case(path))

parseAltitudeDisplayInt :: IO ()
parseAltitudeDisplayInt = do
  res <- parseShan (path altitudeDisplayInt)
  mapM_ printGraph res
  where 
    printGraph g = case g of 
      Left sd -> print sd
      Right automaton -> print automaton

parseSpec :: Spec
parseSpec = do
  describe "parse altitude display with int fragment" $ do 
    it "does not throw exception" $ 
      parseAltitudeDisplayInt