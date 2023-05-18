module Shan.Analysis.Pretty(
  modelValues
) where

import Data.SBV.Internals (SMTModel (modelAssocs))
import Text.Printf (printf)

modelValues :: SMTModel -> String
modelValues m = 
  unlines (showAssoc <$> assocs)
  where
    assocs = modelAssocs m
    showAssoc (s, v) = printf "%s = %s" (show s) (show v)

