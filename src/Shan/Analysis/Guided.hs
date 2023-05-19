module Shan.Analysis.Guided
  ( Index,
    analyzeCase,
    analyzeCases,
  )
where

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), evalStateT)
import Data.Either (partitionEithers)
import Data.SBV (OrdSymbolic, SBool, SReal, SymVal (literal), Symbolic, namedConstraint, runSMT, sNot, sTrue, setOption, (.&&), (./=), (.<), (.<=), (.==), (.>), (.>=), (.||), sOr, sAnd, SWord8, sWord8, sReal)
import Data.SBV.Control (CheckSatResult (..), SMTOption (..), checkSat, getModel, getUnknownReason, getUnsatCore, query)
import Data.SBV.Internals (SMTModel)
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Shan.Analysis.Pretty (modelValues)
import Shan.Analysis.Trace (Direction (..), LMessage, LTrace, Trace, projection, selectEvent, traces, showTrace)
import Shan.Analysis.Validation (validateDiagrams)
import Shan.Ast.Diagram (Assignment (..), Automaton (Automaton), Bound, Dexpr (..), Differential (..), Edge (..), Event (Event), Expr (..), JudgeOp (..), Judgement (..), Message (Message), Name, Node (Node), Property (Property), Reachability (..), Variable, automatonVars, selectEdgeByName, automatonInitialEdges, aname, nname, nonInitialEdges)
import Shan.Parser (parseShan)
import Shan.Util (Case (..))
import Text.Printf (printf)
import Shan.Analysis.UnsatCore (initialName, propertiesName, segmentName)
import Shan.Analysis.LocMap (LocMap, llookup)
import Shan.Analysis.Memo (SymMemo, Index, Memo (locLiteralMap), lookupDuration, insertDuration, lookupLocation, insertLocation, lookupVariable, insertVariable, lookupSyncTime, insertSyncTime, lookupSyncValue, insertSyncValue, emptyMemo)

analyzeCases :: [Case] -> IO ()
analyzeCases = mapM_ analyzeCase

analyzeCase :: Case -> IO ()
analyzeCase c = do
  putStrLn (name c)
  diagrams <- parseShan (path c)
  let (sds, automata) = partitionEithers diagrams
  let validationRes = validateDiagrams (sds, automata)
  case validationRes of
    Left io -> io
    Right _ ->
      let ts = concatMap traces sds
       in analyzeHanGuidedByTraces (bound c) automata ts

analyzeHanGuidedByTraces :: Bound -> [Automaton] -> [Trace] -> IO ()
analyzeHanGuidedByTraces _ _ [] = putStrLn "verified"
analyzeHanGuidedByTraces b ms (t : ts) = do
  putStrLn $ showTrace t
  res <- analyzeHanGuidedByTrace b ms t
  case res of
    Left unsatCore -> do
      print unsatCore
      putStrLn "---------"
      analyzeHanGuidedByTraces b ms ts
    Right counterExample -> putStrLn $ modelValues counterExample

analyzeHanGuidedByTrace :: Bound -> [Automaton] -> Trace -> IO (Either [String] SMTModel)
analyzeHanGuidedByTrace b ms t = do
  runSMT $ querySmtVerificationResult b ms t

querySmtVerificationResult :: Bound 
                           -> [Automaton] 
                           -> Trace 
                           -> Symbolic (Either [String] SMTModel)
querySmtVerificationResult b ms t = do
  evalStateT (encodeAutomataWithProperties b ms t) (emptyMemo ms)
  setOption $ ProduceUnsatCores True
  -- setOption $ OptionKeyword ":smt.core.minimize" ["true"]
  query $ do
    satRes <- checkSat
    case satRes of
      Unsat -> Left <$> getUnsatCore
      Sat -> Right <$> getModel
      DSat Nothing -> error "delta satisfiable"
      DSat (Just s) -> error $ "delta satisfiable: " ++ show s
      Unk -> do
        reason <- getUnknownReason
        error $ "unknown: " ++ show reason

encodeAutomataWithProperties :: Bound -> [Automaton] -> Trace -> SymMemo ()
encodeAutomataWithProperties b ms t = do
  encodeAutomataGuidedByTrace b ms t
  propertiesInNegation <- encodePropertiesInNegation b ms t
  lift $ namedConstraint propertiesName propertiesInNegation

encodePropertiesInNegation :: Bound -> [Automaton] -> Trace -> SymMemo SBool
encodePropertiesInNegation b ms t =
  sNot . sAnd <$> traverse automatonProperties ms
  where
    automatonProperties m@(Automaton n _ _ _ ps) =
      sAnd <$> traverse singleProperty ps
      where
        locationLength = (length (projection t m)) * (b + 1)
        singleProperty (Property node Reachable) = sOr <$> traverse (localize' n node) [0 .. locationLength]
        singleProperty (Property node Unreachable) = sAnd <$> traverse (unlocalize' n node) [0 .. locationLength]

encodeAutomataGuidedByTrace :: Bound -> [Automaton] -> Trace -> SymMemo ()
encodeAutomataGuidedByTrace b ms t = mapM_ (\m -> encodeAutomatonGuidedByTrace b m t) ms

encodeAutomatonGuidedByTrace :: Bound -> Automaton -> Trace -> SymMemo ()
encodeAutomatonGuidedByTrace b m t = encodeAutomataGuidedByLTrace b m (projection t m)

encodeAutomataGuidedByLTrace :: Bound -> Automaton -> LTrace -> SymMemo ()
encodeAutomataGuidedByLTrace b m lt = do
  let indexedLt = zip lt ([1..] :: [Index])
  automatonIsSetToInitialStates <- initialState m
  mapM_ (encodeSegment b m) indexedLt
  lift $ namedConstraint (initialName m) automatonIsSetToInitialStates

encodeSegment :: Bound -> Automaton -> (LMessage, Index) -> SymMemo ()
encodeSegment b m (lm, i) = do
  let n = aname m
  let cursor = offset b i
  localTransitions <- sAnd <$> traverse (transition m) [(cursor + 1) .. (cursor + b)]
  let endIndex = cursor + b + 1
  timeCostIsZero <- zeroTimeCost n endIndex
  let (Event en _) = selectEvent lm
  let edge = selectEdgeByName en m
  endOfSegmentIsSynchronousJump <- synchronousJump m endIndex edge (lm, i)
  timeIsSynchronized <- synchronizeTime n endIndex (lm, i)
  lift $
    namedConstraint
      (segmentName n i)
      ( localTransitions
          .&& timeIsSynchronized
          .&& timeCostIsZero
          .&& endOfSegmentIsSynchronousJump
      )

synchronizeTime :: Name -> Index -> (LMessage, Index) -> SymMemo SBool
synchronizeTime n endIdx ((Message mn _ _ _, _), i) = do
  synchronousMessage <- synchronousTimeVar mn i
  untilNow <- sumOfCostTime
  return (synchronousMessage .== untilNow)
  where
    sumOfCostTime = sum <$> traverse (duration n) [1 .. endIdx]

transition :: Automaton -> Index -> SymMemo SBool
transition m i = do
  jumpToSomeNode <- jump m i
  stayInCurrentNode <- timed m i
  return (jumpToSomeNode .|| stayInCurrentNode)

initialState :: Automaton -> SymMemo SBool
initialState m =
  sOr <$> traverse initialEdge (automatonInitialEdges m)
  where
    n = aname m
    initialEdge (Edge _ _ t _ as) = do
      memo <- get
      let lmap = locLiteralMap memo
      l <- location n 0
      let initialLocation = l .== literalLocation lmap n (nname t)
      variablesAreAssigned <- assignments n as 0
      return (initialLocation .&& variablesAreAssigned)

-- all the variables in the automaton remain unchanged, i.e. v_{i-1} == v_i
unchanged :: Name -> Set Variable -> Index -> SymMemo SBool
unchanged n vs i = 
  sAnd <$> clauses
  where
    clauses = traverse unchanged' (S.toList vs)
    unchanged' :: Variable -> SymMemo SBool
    unchanged' v = do
      pre <- indexedVar n v (i - 1)
      suc <- indexedVar n v i
      return (pre .== suc)

-- update the variables in the automaton according to the assignments
assignments :: Name -> [Assignment] -> Index -> SymMemo SBool
assignments n as i =
  sAnd <$> traverse assignment as
  where
    assignment (Assignment v e) = do
      pre <- encodeExpr n e (max (i - 1) 0)
      suc <- indexedVar n v i
      return (suc .== pre)

judgement :: Name -> Index -> Judgement -> SymMemo SBool
judgement n i = judgement'
  where
    judgement' (SimpleJ le op re) = do
      left <- encodeExpr n le i
      right <- encodeExpr n re i
      return (judge op left right)
    judgement' (AndJ j1 j2) = do
      j1' <- judgement' j1
      j2' <- judgement' j2
      return (j1' .&& j2')
    judgement' (OrJ j1 j2) = do
      j1' <- judgement' j1
      j2' <- judgement' j2
      return (j1' .|| j2')

-- the invariants in the timed transition
-- TODO: ensure the invariant in the whole time duration
invariants :: Name -> [Judgement] -> Index -> SymMemo SBool
invariants n js i = sAnd <$> traverse (judgement n i) js

-- update the variables in the automaton according to the flow conditions
flow :: Name -> [Differential] -> Index -> SymMemo SBool
flow n ds i =
  sAnd <$> traverse flow' ds
  where
    flow' (Differential v op de) = do
      left <- delta v
      right <- encodeDexpr de
      return (judge op left right)
    delta v = do
      vi <- indexedVar n v i
      vi' <- indexedVar n v (i - 1)
      return (vi - vi')
    encodeDexpr :: Dexpr -> SymMemo SReal
    encodeDexpr de =
      case de of
        Dnumber d -> return (literal .fromRational $ toRational d)
        Nvar v -> indexedVar n v i
        Dvar v -> delta v
        Dnegation de' -> negate <$> encodeDexpr de'
        Dadd de1 de2 -> (+) <$> encodeDexpr de1 <*> encodeDexpr de2
        Dsub de1 de2 -> (-) <$> encodeDexpr de1 <*> encodeDexpr de2
        Dmul de1 de2 -> (*) <$> encodeDexpr de1 <*> encodeDexpr de2
        Ddiv de1 de2 -> (/) <$> encodeDexpr de1 <*> encodeDexpr de2

guard :: Name -> Maybe Judgement -> Index -> SymMemo SBool
guard _ Nothing _ = return sTrue
guard n (Just j) i = judgement n i j

-- encoding a jumping transition in an automaton
jump :: Automaton -> Index -> SymMemo SBool
jump m@(Automaton n _ _ es _) i = do
  timeCostIsZero <- zeroTimeCost n i
  allTargetArePossible <- sOr <$> traverse (encodeEdge m i) (nonInitialEdges es)
  return (timeCostIsZero .&& allTargetArePossible)

encodeEdge :: Automaton -> Index -> Edge -> SymMemo SBool
encodeEdge m@(Automaton n _ _ _ _) i (Edge _ s t g as) = do
  previousLocIsSource <- localize n (i - 1) s
  nextLocIsTarget <- localize n i t
  guardIsTrue <- guard n g (i - 1)
  variablesAreAssigned <- assignments n as i
  restVariablesRemainUnchanged <- unchanged n (automatonVars m \\ updatedVars as) i
  return
    ( previousLocIsSource
        .&& nextLocIsTarget
        .&& guardIsTrue
        .&& variablesAreAssigned
        .&& restVariablesRemainUnchanged
    )

synchronousJump :: Automaton -> Index -> Edge -> (LMessage, Index) -> SymMemo SBool
synchronousJump m@(Automaton n _ _ _ _) i (Edge _ s t g as) ((Message mn _ _ sync, d), mi) = do
  previousLocIsSource <- localize n (i - 1) s
  nextLocIsTarget <- localize n i t
  guardIsTrue <- guard n g (i - 1)
  variablesAreAssigned <- case d of
    Sending -> assignments n as i
    Receiving -> assignments n assignmentsNotShadowedBySync i
  variablesAreSynchronized <- sAnd <$> traverse (synchronize d) (zip sync [0 ..])
  restVariablesRemainUnchanged <- case d of
    Sending -> unchanged n (automatonVars m \\ updatedVars as) i
    Receiving -> unchanged n (automatonVars m \\ updatedVars as \\ syncVars) i
  return
    ( previousLocIsSource
        .&& nextLocIsTarget
        .&& guardIsTrue
        .&& variablesAreAssigned
        .&& variablesAreSynchronized
        .&& restVariablesRemainUnchanged
    )
  where
    syncVars = updatedVars sync
    assignmentsNotShadowedBySync = filter (\(Assignment an _) -> an `notElem` syncVars) as
    synchronize Sending (Assignment _ expr, ai) = do
      mv <- synchronousValueVar mn mi ai
      ev <- encodeExpr n expr i
      return (mv .== ev)
    synchronize Receiving (Assignment v _, ai) = do
      assigned <- indexedVar n v i
      mv <- synchronousValueVar mn mi ai
      return (assigned .== mv)

zeroTimeCost :: Name -> Index -> SymMemo SBool
zeroTimeCost n i = do
  d <- duration n i
  return (d .== 0)

localize :: Name -> Index -> Node -> SymMemo SBool
localize n i node = do
  memo <- get
  let lmap = locLiteralMap memo
  l <- location n i
  return (l .== literalLocation lmap n (nname node))

localize' :: Name -> Name -> Index -> SymMemo SBool
localize' n nodeName i = do
  memo <- get
  let lmap = locLiteralMap memo
  l <- location n i
  return (l .== literalLocation lmap n nodeName)

unlocalize' :: Name -> Name -> Index -> SymMemo SBool
unlocalize' n nodeName i = do
  memo <- get
  let lmap = locLiteralMap memo
  l <- location n i
  return (l ./= literalLocation lmap n nodeName)

-- encode a timed transition in an automaton
timed :: Automaton -> Index -> SymMemo SBool
timed m@(Automaton n _ ns _ _) i = do
  timeCostIsGreaterThanZero <- timeCost
  locationRemainsUnchanged <- locUnchange
  allNodesEvolveAccordingToFlow <- sOr <$> traverse encodeNode ns
  return
    ( timeCostIsGreaterThanZero .&&
      locationRemainsUnchanged .&&
      allNodesEvolveAccordingToFlow
    )
  where
    timeCost = do
      d <- duration n i
      return (d .> 0)
    locUnchange = do
      beforeL <- location n (i - 1)
      afterL <- location n i
      return (beforeL .== afterL)
    selectNode nodeName = do
      memo <- get
      let lmap = locLiteralMap memo
      l <- location n (i - 1)
      return (l .== literalLocation lmap n nodeName)
    encodeNode (Node _ nodeName _ diffs invars) = do
      nodeIsSelected <- selectNode nodeName
      variablesAreUpdatedAccordingToFlow <- flow n diffs i
      restVariablesRemainUnchanged <- unchanged n (automatonVars m \\ evovledVars diffs) i
      invariantsHold <- invariants n invars i
      return
        ( nodeIsSelected .&&
          variablesAreUpdatedAccordingToFlow .&&
          restVariablesRemainUnchanged .&&
          invariantsHold
        )

-- declare a fresh symbolic variable
-- to represent the time costed by the i-th transition
duration :: Name -> Index -> SymMemo SReal
duration n i = do
  memo <- get
  case lookupDuration memo n i of
    Just d -> return d
    Nothing -> do
      d <- lift $ sReal (printf "%s|$duration|%d" n i)
      let memo' = insertDuration memo n i d
      put memo'
      return d

-- declare a fresh symbolic variable
-- to represent the node state after the i-th transition
location :: Name -> Index -> SymMemo SWord8
location n i = do
  memo <- get
  case lookupLocation memo n i of
    Just l -> return l
    Nothing -> do
      l <- lift $ sWord8 (printf "%s|$node|%d" n i)
      let memo' = insertLocation memo n i l
      put memo'
      return l

indexedVar :: Name -> Variable -> Index -> SymMemo SReal
indexedVar n v i = do
  memo <- get
  case lookupVariable memo n v i of
    Just d -> return d
    Nothing -> do
      d <- lift $ sReal (printf "%s|%s|%d" n v i)
      let memo' = insertVariable memo n v i d
      put memo'
      return d

-- declare a fresh symbolic variable
-- to represent the time of the i-th synchronous event
synchronousTimeVar :: Name -> Index -> SymMemo SReal
synchronousTimeVar n i = do
  memo <- get
  case lookupSyncTime memo n i of
    Just d -> return d
    Nothing -> do
      d <- lift $ sReal (printf "$syncTime|%s|%d" n i)
      let memo' = insertSyncTime memo n i d
      put memo'
      return d

-- declare a fresh symbolic variable
-- to represent the exchanged value of i'-th assignment of the i-th synchronous event
synchronousValueVar :: Name -> Index -> Index -> SymMemo SReal
synchronousValueVar n i i' = do
  memo <- get
  case lookupSyncValue memo n i i' of
    Just d -> return d
    Nothing -> do
      d <- lift $ sReal (printf "$syncValue|%s|%d|%d" n i i')
      let memo' = insertSyncValue memo n i i' d
      put memo'
      return d

encodeExpr :: Name -> Expr -> Index -> SymMemo SReal
encodeExpr n e i =
  case e of
    Number d -> return (literal .fromRational $ toRational d)
    Var v -> indexedVar n v i
    Negation e' -> negate <$> encodeExpr n e' i
    Add e1 e2 -> (+) <$> encodeExpr n e1 i <*> encodeExpr n e2 i
    Sub e1 e2 -> (-) <$> encodeExpr n e1 i <*> encodeExpr n e2 i
    Mul e1 e2 -> (*) <$> encodeExpr n e1 i <*> encodeExpr n e2 i
    Div e1 e2 -> (/) <$> encodeExpr n e1 i <*> encodeExpr n e2 i

updatedVars :: [Assignment] -> Set Variable
updatedVars = S.fromList . fmap (\(Assignment v _) -> v)

evovledVars :: [Differential] -> Set Variable
evovledVars = S.fromList . fmap (\(Differential v _ _) -> v)

judge :: OrdSymbolic a => JudgeOp -> a -> a -> SBool
judge Ge = (.>=)
judge Gt = (.>)
judge Le = (.<=)
judge Lt = (.<)
judge Eq = (.==)
judge Neq = (./=)

literalLocation :: LocMap -> Name -> Name -> SWord8
literalLocation lmap n nn = literal (lmap `llookup` (n, nn))

offset :: Bound -> Index -> Int
offset b i = (b + 1) * (i - 1)