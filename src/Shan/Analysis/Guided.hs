module Shan.Analysis.Guided(
  analyzeCase,
  analyzeCases
) where

import Shan.Util (Case (..))
import Shan.Parser (parseShan)
import Data.Either (partitionEithers)
import Shan.Analysis.Trace (traces, Trace, projection, LTrace, LMessage, selectEvent, Direction (..))
import Shan.Ast.Diagram (Bound, Variable, Name, Automaton (Automaton), Assignment (..), Expr (..), Differential (..), JudgeOp (..), Dexpr (..), Judgement (..), Edge (..), Node (Node), automatonVars, Event (Event), selectEdgeByName, Message (Message), Reachability (..), Property (Property))
import Shan.Analysis.Validation (validateDiagrams)
import Data.SBV (SBool, SDouble, sDouble, Symbolic, (.==), sTrue, (.&&), SymVal (literal), SString, sString, OrdSymbolic, (.>=), (.>), (.<=), (.<), (./=), (.||), sFalse, setOption, constrain, namedConstraint)
import Text.Printf (printf)
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Data.Text (unpack)
import Data.SBV.Trans (sat)
import Data.SBV.Control (SMTOption(ProduceUnsatCores))

type Index = Int

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
    Right _ -> let ts = concatMap traces sds
                in analyzeHanGuidedByTraces (bound c) automata ts

analyzeHanGuidedByTraces :: Bound -> [Automaton] -> [Trace] -> IO ()
analyzeHanGuidedByTraces b ms = mapM_ (analyzeHanGuidedByTrace b ms)

analyzeHanGuidedByTrace :: Bound -> [Automaton] -> Trace -> IO ()
analyzeHanGuidedByTrace b ms t = do
  let allFormulae = encodeAutomataWithProperties b ms t
  result <- sat allFormulae
  undefined

querySmtVerificationResult :: Bound -> [Automaton] -> Trace -> Symbolic (Either String ())
querySmtVerificationResult b ms t = do
  setOption $ ProduceUnsatCores True
  allFormulae <- encodeAutomataWithProperties b ms t
  constrain allFormulae
  undefined

encodeAutomataWithProperties :: Bound -> [Automaton] -> Trace -> Symbolic SBool
encodeAutomataWithProperties b ms t = do
  encodedAutomata <- encodeAutomataGuidedByTrace b ms t
  propertiesInNegation <- encodePropertiesInNegation b ms t
  -- namedConstraint "automataGuidedByTrace" encodedAutomata
  -- namedConstraint "propertiesInNegation" propertiesInNegation
  return (encodedAutomata .&& propertiesInNegation)

encodePropertiesInNegation :: Bound -> [Automaton] -> Trace -> Symbolic SBool
encodePropertiesInNegation b ms t =
  foldl (.&&) sTrue <$> traverse automatonProperties ms
  where
    automatonProperties m@(Automaton n _ _ _ ps) = 
      foldl (.&&) sTrue <$> traverse singleProperty ps
      where
        locationLength = (length (projection t m)) * (b + 1)
        singleProperty (Property node Reachable) = 
          foldl (.||) sFalse
          <$>
          traverse (localize' n node) [0..locationLength]
        singleProperty (Property node Unreachable) = 
          foldl (.&&) sTrue
          <$>
          traverse (localize' n node) [0..locationLength]

encodeAutomataGuidedByTrace :: Bound -> [Automaton] -> Trace -> Symbolic SBool
encodeAutomataGuidedByTrace b ms t = do
  foldl (.&&) sTrue
  <$>
  traverse (\m -> encodeAutomatonGuidedByTrace b m t) ms

encodeAutomatonGuidedByTrace :: Bound -> Automaton -> Trace -> Symbolic SBool
encodeAutomatonGuidedByTrace b m t = encodeAutomataGuidedByLTrace b m (projection t m)

encodeAutomataGuidedByLTrace :: Bound -> Automaton -> LTrace -> Symbolic SBool
encodeAutomataGuidedByLTrace b m lt = do
  let indexedLt = zip lt ([0..]::[Index])
  automatonIsSetToInitialStates <- initialState m
  automatonEvovleGuidedBySegments <- foldl (.&&) sTrue <$> traverse (encodeSegment b m) indexedLt
  return (automatonIsSetToInitialStates .&& automatonEvovleGuidedBySegments)

encodeSegment :: Bound -> Automaton -> (LMessage, Index) -> Symbolic SBool
encodeSegment b m@(Automaton n _ _ _ _) (lm, i) = do
  let cursor = offset b i
  localTransitions <- foldl (.&&) sTrue
                      <$>
                      traverse (transition m) [(cursor + 1)..(cursor + b)]
  timeCostIsZero <- zeroTimeCost n (cursor + b + 1)
  let (Event en _) = selectEvent lm
  let edge = selectEdgeByName en m
  endOfSegmentIsSynchronousJump <- synchronousJump m i edge (lm, i)
  timeIsSynchronized <- synchronizeTime n (cursor + b + 1) (lm, i)
  return (localTransitions .&&
          timeIsSynchronized .&&
          timeCostIsZero .&&
          endOfSegmentIsSynchronousJump)

synchronizeTime :: Name -> Index -> (LMessage, Index) -> Symbolic SBool
synchronizeTime n endIdx ((Message mn _ _ _, _), i) = do
  synchronousMessage <- synchronousTimeVar mn i
  untilNow <- sumOfCostTime
  return (synchronousMessage .== untilNow)
  where
    sumOfCostTime = sum <$> traverse (duration n) [1..endIdx]

transition :: Automaton -> Index -> Symbolic SBool
transition m i = do
  jumpToSomeNode <- jump m i
  stayInCurrentNode <- timed m i
  return (jumpToSomeNode .|| stayInCurrentNode)

initialState :: Automaton -> Symbolic SBool
initialState (Automaton n _ _ es _) = do
  foldl (.||) sFalse <$> traverse initialEdge es
  where
    initialEdge (Edge _ _ t _ as) = do
      l <- location n 0
      let initialLocation = l .== literalLocation t
      variablesAreAssigned <- assignments n as 0
      return (initialLocation .&& variablesAreAssigned)

-- all the variables in the automaton remain unchanged, i.e. v_{i-1} == v_i
unchanged :: Name -> Set Variable -> Index -> Symbolic SBool
unchanged n vs i =
  foldl (.&&) sTrue <$> clauses
  where
    clauses = traverse unchanged' (S.toList vs)
    unchanged' :: Variable -> Symbolic SBool
    unchanged' s = do
      pre <- indexedVar n s (i - 1)
      suc <- indexedVar n s i
      return (pre .== suc)

-- update the variables in the automaton according to the assignments
assignments :: Name -> [Assignment] -> Index -> Symbolic SBool
assignments n as i =
  foldl (.&&) sTrue <$> traverse assignment as
  where
    assignment (Assignment v e) = do
      pre <- encodeExpr n e (i - 1)
      suc <- indexedVar n v i
      return (suc .== pre)

judgement :: Name -> Index -> Judgement -> Symbolic SBool
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
invariants :: Name -> [Judgement] -> Index -> Symbolic SBool
invariants n js i = foldl (.&&) sTrue <$> traverse (judgement n i) js

-- update the variables in the automaton according to the flow conditions
flow :: Name -> [Differential] -> Index -> Symbolic SBool
flow n ds i =
  foldl (.&&) sTrue <$> traverse flow' ds
  where
    flow' (Differential v op de) = do
      left <- delta v
      right <- encodeDexpr de
      return (judge op left right)
    delta v = do
      vi <- indexedVar n v i
      vi' <- indexedVar n v (i - 1)
      return (vi - vi')
    encodeDexpr ::  Dexpr -> Symbolic SDouble
    encodeDexpr de =
      case de of
        Dnumber d -> return (literal d)
        Nvar v -> indexedVar n v i
        Dvar v -> delta v
        Dnegation de' -> negate <$> encodeDexpr de'
        Dadd de1 de2 -> (+) <$> encodeDexpr de1 <*> encodeDexpr de2
        Dsub de1 de2 -> (-) <$> encodeDexpr de1 <*> encodeDexpr de2
        Dmul de1 de2 -> (*) <$> encodeDexpr de1 <*> encodeDexpr de2
        Ddiv de1 de2 -> (/) <$> encodeDexpr de1 <*> encodeDexpr de2

guard :: Name -> Maybe Judgement -> Index -> Symbolic SBool
guard _ Nothing _ = return sTrue
guard n (Just j) i = judgement n i j

-- encoding a jumping transition in an automaton
jump :: Automaton -> Index -> Symbolic SBool
jump m@(Automaton n _ _ es _) i = do
  timeCostIsZero <- zeroTimeCost n i
  allTargetArePossible <- foldl (.||) sFalse
                          <$>
                          traverse (encodeEdge m i) es
  return (timeCostIsZero .&& allTargetArePossible)

encodeEdge :: Automaton -> Index -> Edge -> Symbolic SBool
encodeEdge m@(Automaton n _ _ _ _) i (Edge _ s t g as) = do
  previousLocIsSource <- localize n (i - 1) s
  nextLocIsTarget <- localize n i t
  guardIsTrue <- guard n g (i - 1)
  variablesAreAssigned <- assignments n as i
  restVariablesRemainUnchanged <- unchanged n (automatonVars m \\ updatedVars as) i
  return (previousLocIsSource .&&
          nextLocIsTarget .&&
          guardIsTrue .&&
          variablesAreAssigned .&&
          restVariablesRemainUnchanged)

synchronousJump :: Automaton -> Index -> Edge -> (LMessage, Index) -> Symbolic SBool
synchronousJump m@(Automaton n _ _ _ _) i (Edge _ s t g as) ((Message mn _ _ sync, d), mi) = do
  previousLocIsSource <- localize n (i - 1) s
  nextLocIsTarget <- localize n i t
  guardIsTrue <- guard n g (i - 1)
  variablesAreAssigned <- case d of
                            Sending -> assignments n as i
                            Receiving -> assignments n assignmentsNotShadowedBySync i
  variablesAreSynchronized <- foldl (.&&) sTrue <$> traverse (synchronize d) (zip sync [0..])
  restVariablesRemainUnchanged <- case d of
                                    Sending -> unchanged n (automatonVars m \\ updatedVars as) i
                                    Receiving -> unchanged n (automatonVars m \\ updatedVars as \\ syncVars) i
  return (previousLocIsSource .&&
          nextLocIsTarget .&&
          guardIsTrue .&&
          variablesAreAssigned .&&
          variablesAreSynchronized .&&
          restVariablesRemainUnchanged)
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

zeroTimeCost :: Name -> Index -> Symbolic SBool
zeroTimeCost n i = do
  d <- duration n i
  return (d .== 0)

localize :: Name -> Index -> Node -> Symbolic SBool
localize n i node = do
  l <- location n i
  return (l .== literalLocation node)

localize' :: Name -> Name -> Index -> Symbolic SBool
localize' n nodeName i = do
  l <- location n i
  return (l .== literal (unpack nodeName))

-- encode a timed transition in an automaton
timed :: Automaton -> Index -> Symbolic SBool
timed m@(Automaton n _ ns _ _) i = do
  timeCostIsGreaterThanZero <- timeCost
  locationRemainsUnchanged <- locUnchange
  allNodesEvolveAccordingToFlow <- foldl (.||) sFalse <$> traverse encodeNode ns
  return (timeCostIsGreaterThanZero .&&
          locationRemainsUnchanged .&&
          allNodesEvolveAccordingToFlow)
  where
    timeCost = do
      d <- duration n i
      return (d .> 0)
    locUnchange = do
      beforeL <- location n (i - 1)
      afterL <- location n i
      return (beforeL .== afterL)
    selectNode nodeName = do
      l <- location n (i - 1)
      return (l .== literal (unpack nodeName))
    encodeNode (Node _ nodeName _ diffs invars) = do
      nodeIsSelected <- selectNode nodeName
      variablesAreUpdatedAccordingToFlow <- flow n diffs i
      restVariablesRemainUnchanged <- unchanged n (automatonVars m \\ evovledVars diffs) i
      invariantsHold <- invariants n invars i
      return (nodeIsSelected .&&
              variablesAreUpdatedAccordingToFlow .&&
              restVariablesRemainUnchanged .&&
              invariantsHold)

-- declare a fresh symbolic variable
-- to represent the time costed by the i-th transition
duration :: Name -> Index -> Symbolic SDouble
duration n i = sDouble (printf "%s-$duration-%d" n i)

-- declare a fresh symbolic variable
-- to represent the node state after the i-th transition
location :: Name -> Index -> Symbolic SString
location n i = sString (printf "%s-$node-%d" n i)

indexedVar :: Name -> Variable -> Index -> Symbolic SDouble
indexedVar n v i = sDouble (printf "%s-%s-%d" n v i)

-- declare a fresh symbolic variable
-- to represent the time of the i-th synchronous event
synchronousTimeVar :: Name -> Index -> Symbolic SDouble
synchronousTimeVar n i = sDouble (printf "$syncTime-%s-%d" n i)

-- declare a fresh symbolic variable
-- to represent the exchanged value of i'-th assignment of the i-th synchronous event 
synchronousValueVar :: Name -> Index -> Index -> Symbolic SDouble
synchronousValueVar n i i' = sDouble (printf "$syncValue-%s-%d-%d" n i i')

encodeExpr :: Name -> Expr -> Index -> Symbolic SDouble
encodeExpr n e i =
  case e of
    Number d -> return (literal d)
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

literalLocation ::Node -> SString
literalLocation (Node _ nn _ _ _) = literal (unpack nn)

offset :: Bound -> Index -> Int
offset b i = (b + 1) * (i - 1)