module Shan.Ast.Diagram
  (
    JudgeOp(..),
    Name,
    Scope,
    Priority,
    Bound,
    Variable(..),
    Expr(..),
    Dexpr(..),
    Judgement(..),
    Assignment(..),
    Differential(..),
    Instance(..),
    Event(..),
    Message(..),
    Item(..),
    Fragment(..),
    IntFragment(..),
    NodeType(..),
    Node(..),
    Edge(..),
    SequenceDiagram(..),
    Reachability(..),
    Property(..),
    Automaton(..),
    Diagrams,
    neg,
    differentialVars,
    judgementVars,
    splitSequenceDiagram
  )
where

import Data.Text (Text)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Maybe (mapMaybe, fromMaybe)


data JudgeOp
  = Ge | Gt | Le | Lt | Eq | Neq
  deriving (Eq, Show)

type Name = Text

type Scope = Text

type Priority = Int

type Bound = Int

data Variable
  = SimpleVariable Name
  | ScopedVariable [Scope] Name
  deriving (Eq, Show, Ord)

data Expr
  = Number Double
  | Var Variable
  | Negation Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

data Dexpr
  = Dnumber Double
  | Nvar Variable
  | Dvar Variable
  | Dnegation Dexpr
  | Dadd Dexpr Dexpr
  | Dsub Dexpr Dexpr
  | Dmul Dexpr Dexpr
  | Ddiv Dexpr Dexpr
  deriving (Eq, Show)

data Judgement
  = SimpleJ Expr JudgeOp Expr
  | AndJ Judgement Judgement
  | OrJ Judgement Judgement
  deriving (Eq, Show)

data Assignment
  = Assignment Variable Expr
  deriving (Eq, Show)

data Differential
  = Differential Variable JudgeOp Dexpr
  deriving (Eq, Show)

data Instance
  = Instance Name Variable
  deriving (Eq, Show)

data Event
  = Event Name Instance
  deriving (Eq, Show)

data Message
  = Message Name Event Event [Assignment]
  deriving (Eq, Show)

data Item
  = ItemM Message
  | ItemF Fragment
  deriving (Eq, Show)

data Fragment
  = Block [Item]
  | AltF [Item] [Item]
  | IntF IntFragment
  | LoopF Bound Bound (Maybe Double) (Maybe Double) [Item]
  deriving (Eq, Show)

data IntFragment
  = IntFragment Priority Bound Bound [Item]
  deriving (Eq, Show)

data NodeType
  = Initial | Common
  deriving (Eq, Show)

data Node
  = Node NodeType Name (Set Variable) [Differential] [Judgement]
  deriving (Eq, Show)

data Edge
  = Edge Name Node Node (Maybe Judgement) [Assignment]
  deriving (Eq, Show)

data SequenceDiagram
  = SequenceDiagram Name [Instance] Fragment [Judgement]
  deriving (Eq, Show)

data Reachability
  = Reachable | Unreachable
  deriving (Eq, Show)

data Property
  = Property Name Reachability
  deriving (Eq, Show)

data Automaton
  = Automaton Name Node [Node] [Edge] [Property]
  deriving (Eq, Show)

type Diagrams = ([SequenceDiagram], [Automaton])

negateOp :: JudgeOp -> JudgeOp
negateOp Eq = Neq
negateOp Neq = Eq
negateOp Gt = Le
negateOp Ge = Lt
negateOp Lt = Ge
negateOp Le = Gt

neg :: Judgement -> Judgement
neg (SimpleJ e1 op e2) = SimpleJ e1 (negateOp op) e2
neg (AndJ j1 j2) = OrJ (neg j1) (neg j2)
neg (OrJ j1 j2) = AndJ (neg j1) (neg j2)

exprVars :: Expr -> Set Variable
exprVars (Number _) = S.empty
exprVars (Var v) = S.singleton v
exprVars (Negation e) = exprVars e
exprVars (Add e1 e2) = S.union (exprVars e1) (exprVars e2)
exprVars (Sub e1 e2) = S.union (exprVars e1) (exprVars e2)
exprVars (Mul e1 e2) = S.union (exprVars e1) (exprVars e2)
exprVars (Div e1 e2) = S.union (exprVars e1) (exprVars e2)

dexprVars :: Dexpr -> Set Variable
dexprVars (Dnumber _) = S.empty
dexprVars (Nvar v) = S.singleton v
dexprVars (Dvar v) = S.singleton v
dexprVars (Dnegation e) = dexprVars e
dexprVars (Dadd e1 e2) = S.union (dexprVars e1) (dexprVars e2)
dexprVars (Dsub e1 e2) = S.union (dexprVars e1) (dexprVars e2)
dexprVars (Dmul e1 e2) = S.union (dexprVars e1) (dexprVars e2)
dexprVars (Ddiv e1 e2) = S.union (dexprVars e1) (dexprVars e2)

differentialVars :: Differential -> Set Variable
differentialVars (Differential v _ e) = S.insert v (dexprVars e)

judgementVars :: Judgement -> Set Variable
judgementVars (SimpleJ e1 _ e2) = S.union (exprVars e1) (exprVars e2)
judgementVars (AndJ j1 j2) = S.union (judgementVars j1) (judgementVars j2)
judgementVars (OrJ j1 j2) = S.union (judgementVars j1) (judgementVars j2)

splitSequenceDiagram :: SequenceDiagram -> (Fragment, [IntFragment])
splitSequenceDiagram (SequenceDiagram _ _ frag _) = (fromMaybe (Block []) (clean frag), ints frag)
    
clean :: Fragment -> Maybe Fragment
clean (Block items) = Just $ Block (mapMaybe noInt items)
clean (AltF items1 items2) = Just $ AltF (mapMaybe noInt items1) (mapMaybe noInt items2)
clean (IntF {}) = Nothing
clean (LoopF l h s i items) = Just $ LoopF l h s i (mapMaybe noInt items)

noInt :: Item -> Maybe Item
noInt i@(ItemM _) = Just i
noInt (ItemF frag) = ItemF <$> clean frag

ints :: Fragment -> [IntFragment]
ints (Block items) = onlyInt items
ints (AltF items1 items2) = onlyInt items1 ++ onlyInt items2
ints (IntF i) = [i]
ints (LoopF _ _ _ _ items) = onlyInt items

onlyInt :: [Item] -> [IntFragment]
onlyInt = concatMap onlyInt'

onlyInt' :: Item -> [IntFragment]
onlyInt' (ItemM _) = []
onlyInt' (ItemF frag) = ints frag
