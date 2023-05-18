module Shan.Analysis.Memo(
  Index,
  Memo(..),
  SymMemo,
  emptyMemo,
  lookupDuration,
  insertDuration,
  lookupLocation,
  insertLocation,
  lookupVariable,
  insertVariable,
  lookupSyncTime,
  insertSyncTime,
  lookupSyncValue,
  insertSyncValue
) where

import Shan.Ast.Diagram (Name, Variable, Automaton)
import Data.Map (Map)
import Data.Map qualified as M
import Data.SBV (SDouble, SWord8, Symbolic)
import Shan.Analysis.LocMap (LocMap, constructMap)
import Control.Monad.State (StateT)

type Index = Int

data Memo = Memo
  { durationMap :: Map (Name, Index) SDouble,
    locationMap :: Map (Name, Index) SWord8,
    variableMap :: Map (Name, Variable, Index) SDouble,
    syncTimeMap :: Map (Name, Index) SDouble,
    syncValueMap :: Map (Name, Index, Index) SDouble,
    locLiteralMap :: LocMap
  }
  deriving (Show)

type SymMemo a = StateT Memo Symbolic a

emptyMemo :: [Automaton] -> Memo
emptyMemo ms = Memo M.empty M.empty M.empty M.empty M.empty (constructMap ms)

lookupDuration :: Memo -> Name -> Index -> Maybe SDouble
lookupDuration memo n index = M.lookup (n, index) (durationMap memo)

insertDuration :: Memo -> Name -> Index -> SDouble -> Memo
insertDuration memo n index value =
  memo {durationMap = M.insert (n, index) value (durationMap memo)}

lookupLocation :: Memo -> Name -> Index -> Maybe SWord8
lookupLocation memo n index = M.lookup (n, index) (locationMap memo)

insertLocation :: Memo -> Name -> Index -> SWord8 -> Memo
insertLocation memo n index value =
  memo {locationMap = M.insert (n, index) value (locationMap memo)}

lookupVariable :: Memo -> Name -> Variable -> Index -> Maybe SDouble
lookupVariable memo n variable index =
  M.lookup (n, variable, index) (variableMap memo)

insertVariable :: Memo -> Name -> Variable -> Index -> SDouble -> Memo
insertVariable memo n variable index value =
  memo {variableMap = M.insert (n, variable, index) value (variableMap memo)}

lookupSyncTime :: Memo -> Name -> Index -> Maybe SDouble
lookupSyncTime memo n index = M.lookup (n, index) (syncTimeMap memo)

insertSyncTime :: Memo -> Name -> Index -> SDouble -> Memo
insertSyncTime memo n index value =
  memo {syncTimeMap = M.insert (n, index) value (syncTimeMap memo)}

lookupSyncValue :: Memo -> Name -> Index -> Index -> Maybe SDouble
lookupSyncValue memo n index1 index2 =
  M.lookup (n, index1, index2) (syncValueMap memo)

insertSyncValue :: Memo -> Name -> Index -> Index -> SDouble -> Memo
insertSyncValue memo n index1 index2 value =
  memo {syncValueMap = M.insert (n, index1, index2) value (syncValueMap memo)}