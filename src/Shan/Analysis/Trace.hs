module Shan.Analysis.Trace
  ( Trace,
    Direction(..),
    LMessage,
    LTrace,
    traces,
    showTrace,
    projection,
    selectEvent
  )
where

import Data.List (sortOn)
import Data.Text qualified as T
import Data.Map ((!))
import Data.Map qualified as M
import Data.Universe.Helpers (cartesianProduct)
import Shan.Ast.Diagram (Fragment (..), IntFragment (IntFragment), Item (ItemF, ItemM), Message (Message), Priority, SequenceDiagram, splitSequenceDiagram, Automaton (Automaton), ename, Event (Event), Instance (Instance))
import Data.Maybe (mapMaybe)

type Trace = [Message]

data Direction
  = Sending | Receiving
  deriving (Eq, Show)

type LMessage = (Message, Direction)

type LTrace = [LMessage]

traces :: SequenceDiagram -> [Trace]
traces sd =
  let (clean, ints) = splitSequenceDiagram sd
      sortedInts = sortByPriority ints
      cleanTraces = companyWithPriority <$> fragTraces clean
      intSeqs = intTraces sortedInts
   in if null intSeqs
        then fst <$> cleanTraces
        else fst <$> concatMap (interrupts cleanTraces) intSeqs

intTraces :: [IntFragment] -> [[([Trace], Priority)]]
intTraces ints = cartesianProductList (intTrace <$> ints)
  where
    intTrace (IntFragment p l h items) = (\i -> replicate i (blockTraces items, p)) <$> [l .. h]

cartesianProductList :: [[[a]]] -> [[a]]
cartesianProductList [] = []
cartesianProductList [s] = s
cartesianProductList (s : sx) = cartesianProduct (++) s (cartesianProductList sx)

sortByPriority :: [IntFragment] -> [IntFragment]
sortByPriority = sortOn priority
  where
    priority (IntFragment p _ _ _) = p

companyWithPriority :: Trace -> (Trace, M.Map Int Priority)
companyWithPriority t =
  let len = length t
      list = [(i, 0) | i <- [0 .. len]]
   in (t, M.fromList list)

fragTraces :: Fragment -> [Trace]
fragTraces (Block items) = blockTraces items
fragTraces (AltF items1 items2) = blockTraces items1 ++ blockTraces items2
fragTraces (LoopF l h _ _ items) = loopTraces [l .. h] (blockTraces items)
fragTraces (IntF (IntFragment _ _ _ items)) = blockTraces items

interrupts :: [(Trace, M.Map Int Priority)] -> [([Trace], Priority)] -> [(Trace, M.Map Int Priority)]
interrupts ts [] = ts
interrupts ts [int] = concatMap (`interrupt` int) ts
interrupts ts (int : ints) = interrupts (concatMap (`interrupt` int) ts) ints

interrupt :: (Trace, M.Map Int Priority) -> ([Trace], Priority) -> [(Trace, M.Map Int Priority)]
interrupt t int = concatMap (interrupt' t int) [0 .. (length $ fst t)]

interrupt' :: (Trace, M.Map Int Priority) -> ([Trace], Priority) -> Int -> [(Trace, M.Map Int Priority)]
interrupt' (interrupted, priorityMap) (ts, priority) point =
  if priority > pointPriority
    then interruptSingle <$> ts
    else []
  where
    pointPriority = priorityMap ! point
    interruptSingle t = (insertListAt point t interrupted, updateMap t)
    list = M.toList priorityMap
    updateMap t =
      let len = length t
          updateKey (i, v) =
            if i < point
              then (i, v)
              else (i + len, v)
          origin = updateKey <$> list
          newList = (point, pointPriority) : ((, priority) <$> [(point + 1) .. (point + len - 1)])
       in M.fromList (origin ++ newList)

insertListAt :: Int -> [a] -> [a] -> [a]
insertListAt index toInsert list = take index list ++ toInsert ++ drop index list

loopTraces :: [Int] -> [Trace] -> [Trace]
loopTraces bounds ts =
  concatMap loop bounds
  where
    loop i = composition (replicate i ts)

blockTraces :: [Item] -> [Trace]
blockTraces items = composition (itemTraces <$> items)

composition :: [[Trace]] -> [Trace]
composition [] = []
composition [s] = s
composition (s : sx) = cartesianProduct (++) s (composition sx)

itemTraces :: Item -> [Trace]
itemTraces (ItemM m) = [[m]]
itemTraces (ItemF frag) = fragTraces frag

showTrace :: Trace -> String
showTrace [] = ""
showTrace [m] = showMessage m
showTrace (m:ms) = showMessage m ++ " -> " ++ showTrace ms

showMessage :: Message -> String
showMessage (Message n _ _ _) = T.unpack n

projection :: Trace -> Automaton -> LTrace
projection t (Automaton aname _ _ es _) =
  mapMaybe projection' t
  where
    enames = ename <$> es
    projection' :: Message -> Maybe LMessage
    projection'
      m@(Message _ (Event sname (Instance saname _)) (Event tname (Instance taname _)) _)
      | saname == aname && sname `elem` enames = Just (m, Sending)
      | taname == aname && tname `elem` enames = Just (m, Receiving)
      | otherwise = Nothing

selectEvent :: LMessage -> Event
selectEvent (Message _ s _ _, Sending) = s
selectEvent (Message _ _ r _, Receiving) = r