module Shan.AST.Diagram
  (
    JudgeOp(..),
    Name,
    Scope,
    Priority,
    Bound,
    Variable(..),
    Expr(..),
    Judgement(..),
    Assignment(..),
    Differential(..),
    Instance(..),
    Message(..),
    Item(..),
    Fragment(..),
    NodeType(..),
    Node(..),
    Edge(..),
    SequenceDiagram(..),
    Automaton(..),
    Diagram(..),
    neg,
    extractDifferentialVariables,
    extractJudgementVariables
  )
where

import Data.Text (Text)
import Data.Set (Set)
import Data.Set qualified as S

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
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
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
  = Differential Variable JudgeOp Expr
  deriving (Eq, Show)

data Instance 
  = Instance Name Variable 
  deriving (Eq, Show)

data Message 
  = Message Name Instance Instance [Assignment]
  deriving (Eq, Show)

data Item 
  = ItemM Message
  | ItemF Fragment
  deriving (Eq, Show)

data Fragment
  = Block [Item]
  | AltF [Item] [Item]
  | IntF Priority Bound Bound [Item]
  | LoopF Bound Bound (Maybe Double) (Maybe Double) [Item]
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

data Automaton
  = Automaton Name Node [Node] [Edge] [Judgement]
  deriving (Eq, Show)

class Diagram a where
  title :: a -> Name
  properties :: a -> [Judgement]

instance Diagram SequenceDiagram where
  title (SequenceDiagram n _ _ _) = n
  properties (SequenceDiagram _ _ _ ps) = ps

instance Diagram Automaton where
  title (Automaton n _ _ _ _) = n
  properties (Automaton _ _ _ _ ps) = ps

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

extractExprVariables :: Expr -> Set Variable
extractExprVariables (Number _) = S.empty
extractExprVariables (Var v) = S.singleton v
extractExprVariables (Add e1 e2) = S.union (extractExprVariables e1) (extractExprVariables e2)
extractExprVariables (Sub e1 e2) = S.union (extractExprVariables e1) (extractExprVariables e2)
extractExprVariables (Mul e1 e2) = S.union (extractExprVariables e1) (extractExprVariables e2)
extractExprVariables (Div e1 e2) = S.union (extractExprVariables e1) (extractExprVariables e2)

extractDifferentialVariables :: Differential -> Set Variable
extractDifferentialVariables (Differential v _ e) = S.insert v (extractExprVariables e)

extractJudgementVariables :: Judgement -> Set Variable
extractJudgementVariables (SimpleJ e1 _ e2) = S.union (extractExprVariables e1) (extractExprVariables e2)
extractJudgementVariables (AndJ j1 j2) = S.union (extractJudgementVariables j1) (extractJudgementVariables j2)
extractJudgementVariables (OrJ j1 j2) = S.union (extractJudgementVariables j1) (extractJudgementVariables j2)