{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Shan.Ast.Diagram.Parser
  ( parseDiagram,
    parseSequenceDiagram,
    parseAutomaton,
    parseJudgement,
    judgeOpParser,
  )
where

import Control.Applicative.Combinators (many)
import Control.Lens ((^.), _Just)
import Control.Monad (void)
import Control.Monad.Combinators ((<|>))
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Shan.Ast.Diagram
  ( Assignment (Assignment),
    Automaton (Automaton),
    Bound,
    Dexpr (..),
    Differential (Differential),
    Edge (Edge),
    Event (..),
    Expr (..),
    Fragment (..),
    Instance (Instance),
    IntFragment (IntFragment),
    Item (ItemF, ItemM),
    JudgeOp (..),
    Judgement (AndJ, OrJ, SimpleJ),
    Message (Message),
    Name,
    Node (..),
    NodeType (..),
    Priority,
    Property (..),
    Reachability (..),
    SequenceDiagram (SequenceDiagram),
    Variable,
    differentialVars,
    judgementVars,
  )
import Shan.Uxf.Uxf (Basic, DiagramType (..), Element (BasicE, RelationE), RawDiagram (..), Relation, UMLType (..), content, element, elementType, h, sourceX, sourceY, targetX, targetY, w, x, y, (=?))
import Text.Megaparsec (MonadParsec (eof, try), anySingle, between, choice, manyTill, optional, parse, single, (<?>))
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, float, lexeme, symbol)

type Parser a = Mega.Parsec Void Text a

type Location = (Double, Double, Double, Double)

keywords :: [String]
keywords = ["valign"]

parseDiagram :: RawDiagram -> Either Shan.Ast.Diagram.SequenceDiagram Shan.Ast.Diagram.Automaton
parseDiagram d@(RawDiagram _ SD _) = Left $ parseSequenceDiagram d
parseDiagram d@(RawDiagram _ HA _) = Right $ parseAutomaton d

parseSequenceDiagram :: RawDiagram -> Shan.Ast.Diagram.SequenceDiagram
parseSequenceDiagram (RawDiagram _ _ es) =
  let sd = findElementSD es ^. content
      note = findElementNote es ^. _Just . content
      (n, ins, f) = parseSDBody sd
      props = parseConstraints note
   in Shan.Ast.Diagram.SequenceDiagram n ins f props

parseSDBody :: Text -> (Shan.Ast.Diagram.Name, [Shan.Ast.Diagram.Instance], Shan.Ast.Diagram.Fragment)
parseSDBody s = case parse sdBodyParser "" s of
  Left e -> error ("parse sequence diagram failed: " ++ Mega.errorBundlePretty e)
  Right res -> res

parseConstraints :: Text -> [Shan.Ast.Diagram.Judgement]
parseConstraints s = case parse judgementsParser "" s of
  Left e -> error ("parse constraints failed: " ++ Mega.errorBundlePretty e)
  Right res -> res

parseProperties :: Text -> [Shan.Ast.Diagram.Property]
parseProperties note = case parse propertiesParser "" note of
  Left e -> error ("parse properties failed: " ++ Mega.errorBundlePretty e)
  Right properties -> properties

parseAutomaton :: RawDiagram -> Shan.Ast.Diagram.Automaton
parseAutomaton (RawDiagram title _ es) =
  let i@(_, initialNode) = parseInitial $ findInitialNode es
      locNodes = parseNode <$> findNodes es
      nodes = snd <$> locNodes
      locList = i : locNodes
      relations = parseEdge locList <$> findRelations es
      note = findElementNote es ^. _Just . content
      props = parseProperties note
   in Shan.Ast.Diagram.Automaton title initialNode nodes relations props

parseInitial :: Basic -> (Location, Shan.Ast.Diagram.Node)
parseInitial b =
  let loc = parseLocation b
   in (loc, Shan.Ast.Diagram.Node Shan.Ast.Diagram.Initial "" S.empty [] [])

parseEdge :: [(Location, Shan.Ast.Diagram.Node)] -> Relation -> Shan.Ast.Diagram.Edge
parseEdge locs r =
  let source = searchSource locs r
      target = searchTarget locs r
      (n, j, as) = case parse edgeParser "" (r ^. element . content) of
        Left e -> error ("parse edge failed: " ++ Mega.errorBundlePretty e)
        Right res -> res
   in Shan.Ast.Diagram.Edge n source target j as

edgeParser :: Parser (Shan.Ast.Diagram.Name, Maybe Shan.Ast.Diagram.Judgement, [Shan.Ast.Diagram.Assignment])
edgeParser = do
  space
  void (symbolS "lt" *> symbolS "=" *> symbolS "->")
  n <- nameParser
  res <- optional tailParser
  let (j, as) = fromMaybe (Nothing, []) res
  return (n, j, as)
  where
    tailParser :: Parser (Maybe Shan.Ast.Diagram.Judgement, [Shan.Ast.Diagram.Assignment])
    tailParser = do
      space
      void (symbolS ":")
      j <- optional judgementParser
      as <- assignmentsParser
      return (j, as)

searchSource :: [(Location, Shan.Ast.Diagram.Node)] -> Relation -> Shan.Ast.Diagram.Node
searchSource locs r =
  let sx = (r ^. element . x) + (r ^. sourceX)
      sy = (r ^. element . y) + (r ^. sourceY)
   in case searchNode locs (sx, sy) of
        Nothing -> error ("search source node failed: " ++ T.unpack (r ^. element . content))
        Just n -> n

searchTarget :: [(Location, Shan.Ast.Diagram.Node)] -> Relation -> Shan.Ast.Diagram.Node
searchTarget locs r =
  let tx = (r ^. element . x) + (r ^. targetX)
      ty = (r ^. element . y) + (r ^. targetY)
   in case searchNode locs (tx, ty) of
        -- Nothing -> error ("search target node failed: " ++ T.unpack (r ^. element . content))
        Nothing -> error ("search target node failed: " ++ show locs ++ show r)
        Just n -> n

searchNode :: [(Location, Shan.Ast.Diagram.Node)] -> (Double, Double) -> Maybe Shan.Ast.Diagram.Node
searchNode locs loc = snd <$> find (\(l, _) -> inSquare l loc) locs

inSquare :: Location -> (Double, Double) -> Bool
inSquare (x1, x2, y1, y2) (dotx, doty) =
  x1 <= dotx
    && x2 >= dotx
    && y1 <= doty
    && y2 >= doty

parseNode :: Basic -> (Location, Shan.Ast.Diagram.Node)
parseNode b =
  let loc = parseLocation b
      node = parseNodeContent (b ^. content)
   in (loc, node)

parseNodeContent :: Text -> Shan.Ast.Diagram.Node
parseNodeContent s =
  let (n, vset, diffs, judges) = case parse nodeContentParser "" s of
        Left e -> error ("parse node content failed: " ++ Mega.errorBundlePretty e)
        Right res -> res
   in Shan.Ast.Diagram.Node Shan.Ast.Diagram.Common n vset diffs judges

nodeContentParser :: Parser (Shan.Ast.Diagram.Name, S.Set Shan.Ast.Diagram.Variable, [Shan.Ast.Diagram.Differential], [Shan.Ast.Diagram.Judgement])
nodeContentParser = do
  space
  title <- manyTill anySingle newline
  void (symbolS "--")
  diffs <- many differentialParser
  space
  void (symbolS "-.")
  judges <- manyTill judgementParser (optional nodeContentEnd <* eof)
  let diffVars = S.unions (Shan.Ast.Diagram.differentialVars <$> diffs)
  let judgeVars = S.unions (Shan.Ast.Diagram.judgementVars <$> judges)
  return (T.pack title, S.union diffVars judgeVars, diffs, judges)
  where
    nodeContentEnd :: Parser Text
    nodeContentEnd = symbolS "valign" <* symbolS "=" <* symbolS "top"

parseLocation :: Basic -> Location
parseLocation b =
  let xv = b ^. x
      yv = b ^. y
      wv = b ^. w
      hv = b ^. h
   in (xv, xv + wv, yv, yv + hv)

differentialParser :: Parser Shan.Ast.Diagram.Differential
differentialParser = do
  void (symbolS "'")
  v <- variableParser
  op <- judgeOpParser
  dexpr <- dexprParser
  void newline <|> eof
  return $ Shan.Ast.Diagram.Differential v op dexpr

parseJudgement :: Text -> Shan.Ast.Diagram.Judgement
parseJudgement j = case parse judgementParser "" j of
  Left e -> error ("parse judgement failed: " ++ Mega.errorBundlePretty e)
  Right judge -> judge

sdBodyParser :: Parser (Shan.Ast.Diagram.Name, [Shan.Ast.Diagram.Instance], Shan.Ast.Diagram.Fragment)
sdBodyParser = do
  space
  title <- titleParser
  void (many newline)
  ins <- many instanceParser
  void (many newline)
  let insMap = M.fromList ((\i@(Shan.Ast.Diagram.Instance _ v) -> (v, i)) <$> ins)
  items <- itemsParser insMap
  return (title, ins, Shan.Ast.Diagram.Block items)

titleParser :: Parser Shan.Ast.Diagram.Name
titleParser = do
  space
  void (symbolS "title")
  void (symbolS "=")
  n <- nameParser
  void (many newline)
  return n

instanceParser :: Parser Shan.Ast.Diagram.Instance
instanceParser = do
  space
  void (symbolS "obj")
  void (symbolS "=")
  n <- nameParser
  void (symbolS "~")
  v <- nameParser
  void (many newline)
  return $ Shan.Ast.Diagram.Instance n v

itemsParser :: M.Map Shan.Ast.Diagram.Variable Shan.Ast.Diagram.Instance -> Parser [Shan.Ast.Diagram.Item]
itemsParser insMap = many (itemParser insMap)

itemParser :: M.Map Shan.Ast.Diagram.Variable Shan.Ast.Diagram.Instance -> Parser Shan.Ast.Diagram.Item
itemParser insMap = (Shan.Ast.Diagram.ItemF <$> fragmentParser insMap) <|> (Shan.Ast.Diagram.ItemM <$> messageParser insMap)

fragmentParser :: M.Map Shan.Ast.Diagram.Variable Shan.Ast.Diagram.Instance -> Parser Shan.Ast.Diagram.Fragment
fragmentParser insMap =
  choice
    [ try (loopFragmentParser insMap),
      try (altFragmentParser insMap),
      intFragmentParer insMap
    ]

loopFragmentParser :: M.Map Shan.Ast.Diagram.Variable Shan.Ast.Diagram.Instance -> Parser Shan.Ast.Diagram.Fragment
loopFragmentParser insMap = do
  space
  void (symbolS "combinedFragment")
  void (symbolS "=")
  (lower, upper, maybeStart, maybeInterval) <- loopHeader
  void (symbolS "~")
  void (optional nameParser)
  void (many newline)
  items <- itemsParser insMap
  void (symbolS "--")
  void (many newline)
  return (Shan.Ast.Diagram.LoopF lower upper maybeStart maybeInterval items)
  where
    loopHeader :: Parser (Shan.Ast.Diagram.Bound, Shan.Ast.Diagram.Bound, Maybe Double, Maybe Double)
    loopHeader = do
      space
      void (symbolS "loop")
      (lower, upper) <- boundParser
      assigns <- assignmentsParser
      let assignMap = M.fromList ((\(Shan.Ast.Diagram.Assignment v e) -> (v, e)) <$> assigns)
      let start = M.lookup "start" assignMap >>= exprToNumber
      let interval = M.lookup "interval" assignMap >>= exprToNumber
      return (lower, upper, start, interval)
    exprToNumber (Shan.Ast.Diagram.Number n) = Just n
    exprToNumber _ = Nothing

intFragmentParer :: M.Map Shan.Ast.Diagram.Variable Shan.Ast.Diagram.Instance -> Parser Shan.Ast.Diagram.Fragment
intFragmentParer insMap = do
  space
  void (symbolS "combinedFragment")
  void (symbolS "=")
  (p, lower, upper) <- intHeader
  void (symbolS "~")
  void (optional nameParser)
  void (many newline)
  items <- itemsParser insMap
  void (symbolS "--")
  void (many newline)
  return $ Shan.Ast.Diagram.IntF $ Shan.Ast.Diagram.IntFragment p lower upper items
  where
    intHeader :: Parser (Shan.Ast.Diagram.Priority, Shan.Ast.Diagram.Bound, Shan.Ast.Diagram.Bound)
    intHeader = do
      space
      void (symbolS "int")
      void (symbolS "(")
      void (symbolS "p")
      void (symbolS "=")
      p <- lexeme space decimal
      void (symbolS ")")
      maybeBound <- optional boundParser
      let (lower, upper) = fromMaybe (1, 1) maybeBound
      return (p, lower, upper)

altFragmentParser :: M.Map Shan.Ast.Diagram.Variable Shan.Ast.Diagram.Instance -> Parser Shan.Ast.Diagram.Fragment
altFragmentParser insMap = do
  space
  void (symbolS "combinedFragment")
  void (symbolS "=")
  void (symbolS "alt")
  void (symbolS "~")
  void (optional nameParser)
  void (many newline)
  items <- itemsParser insMap
  void (symbolS "..")
  void (many newline)
  altItems <- itemsParser insMap
  void (symbolS "--")
  void (many newline)
  return $ Shan.Ast.Diagram.AltF items altItems

boundParser :: Parser (Shan.Ast.Diagram.Bound, Shan.Ast.Diagram.Bound)
boundParser = do
  void (symbolS "(")
  lower <- lexeme space decimal
  void (symbolS ",")
  upper <- lexeme space decimal
  void (symbolS ")")
  return (lower, upper)

messageParser :: M.Map Shan.Ast.Diagram.Variable Shan.Ast.Diagram.Instance -> Parser Shan.Ast.Diagram.Message
messageParser insMap = do
  space
  sv <- variableParser
  void (symbolS "->>>")
  tv <- variableParser
  void (symbolS ":")
  (n, sn, tn) <- nameTriple
  assigns <- assignmentsParser
  void (optional $ symbolS ";")
  void (many newline)
  let si = case M.lookup sv insMap of
        Just i -> i
        Nothing -> missing (show sv)
  let ti = case M.lookup tv insMap of
        Just i -> i
        Nothing -> missing (show sv)
  return $ Shan.Ast.Diagram.Message n (Shan.Ast.Diagram.Event sn si) (Shan.Ast.Diagram.Event tn ti) assigns
  where
    missing s = error ("instance variable not declared: " ++ s)
    nameTriple :: Parser (Shan.Ast.Diagram.Name, Shan.Ast.Diagram.Name, Shan.Ast.Diagram.Name)
    nameTriple = do
      void (symbolW "(")
      n <- nameParser
      void (symbolW ",")
      sn <- nameParser
      void (symbolW ",")
      tn <- nameParser
      void (symbolW ")")
      return (n, sn, tn)

propertiesParser :: Parser [Shan.Ast.Diagram.Property]
propertiesParser = many propertyParser <?> "properties"

propertyParser :: Parser Shan.Ast.Diagram.Property
propertyParser = do
  void (symbolW "(")
  n <- nameParser
  void (symbolW ",")
  r <- reachabilityParser
  void (symbolS ")")
  return $ Shan.Ast.Diagram.Property n r

reachabilityParser :: Parser Shan.Ast.Diagram.Reachability
reachabilityParser =
  choice
    [ Shan.Ast.Diagram.Reachable <$ symbolS "reachable",
      Shan.Ast.Diagram.Unreachable <$ symbolS "unreachable"
    ] <?> "reachability"

judgementsParser :: Parser [Shan.Ast.Diagram.Judgement]
judgementsParser = many judgementParser <?> "judgements"

judgementParser :: Parser Shan.Ast.Diagram.Judgement
judgementParser =
  (makeExprParser simple table <* space) <?> "judgement"
  where
    simple = do
      l <- exprParser
      op <- judgeOpParser
      r <- exprParser
      return $ Shan.Ast.Diagram.SimpleJ l op r
    table =
      [ [binary "&&" Shan.Ast.Diagram.AndJ],
        [binary "||" Shan.Ast.Diagram.OrJ]
      ]

judgeOpParser :: Parser Shan.Ast.Diagram.JudgeOp
judgeOpParser =
  choice
    [ Shan.Ast.Diagram.Ge <$ symbolS ">=",
      Shan.Ast.Diagram.Gt <$ symbolS ">",
      Shan.Ast.Diagram.Le <$ symbolS "<=",
      Shan.Ast.Diagram.Lt <$ symbolS "<",
      try (Shan.Ast.Diagram.Eq <$ symbolS "=="),
      Shan.Ast.Diagram.Eq <$ symbolS "=",
      Shan.Ast.Diagram.Neq <$ symbolS "!="
    ]
    <?> "judgement operations"

assignmentsParser :: Parser [Shan.Ast.Diagram.Assignment]
assignmentsParser = do
  space
  leftB <- optional (symbolS "[")
  case leftB of
    Nothing -> return []
    Just _ -> do
      fstAssign <- optional assignmentParser
      assigns <- case fstAssign of
        Nothing -> return []
        Just f -> do
          tails <- many tailParser
          return (f : tails)
      void (symbolS "]")
      return assigns
  where
    tailParser :: Parser Shan.Ast.Diagram.Assignment
    tailParser = do
      void (symbolS ",")
      assignmentParser

assignmentParser :: Parser Shan.Ast.Diagram.Assignment
assignmentParser = do
  space
  v <- variableParser
  void (symbolS ":=")
  expr <- exprParser
  return $ Shan.Ast.Diagram.Assignment v expr

dexprParser :: Parser Shan.Ast.Diagram.Dexpr
dexprParser =
  makeExprParser terms table <?> "dexpr"
  where
    terms =
      choice
        [ Shan.Ast.Diagram.Dnumber <$> numberParser,
          Shan.Ast.Diagram.Nvar <$> variableParser,
          Shan.Ast.Diagram.Dvar <$> (symbolS "'" *> variableParser),
          parens dexprParser
        ]
    table =
      [ [ prefix "-" Shan.Ast.Diagram.Dnegation,
          prefix "+" id
        ],
        [ binary "*" Shan.Ast.Diagram.Dmul,
          binary "/" Shan.Ast.Diagram.Ddiv
        ],
        [ binary "+" Shan.Ast.Diagram.Dadd,
          binary "-" Shan.Ast.Diagram.Dsub
        ]
      ]

exprParser :: Parser Shan.Ast.Diagram.Expr
exprParser =
  makeExprParser terms table <?> "expr"
  where
    terms =
      choice
        [ Shan.Ast.Diagram.Number <$> numberParser,
          Shan.Ast.Diagram.Var <$> variableParser,
          parens exprParser
        ]
    table =
      [ [ prefix "-" Shan.Ast.Diagram.Negation,
          prefix "+" id
        ],
        [ binary "*" Shan.Ast.Diagram.Mul,
          binary "/" Shan.Ast.Diagram.Div
        ],
        [ binary "+" Shan.Ast.Diagram.Add,
          binary "-" Shan.Ast.Diagram.Sub
        ]
      ]

binary ::
  Text ->
  (a -> a -> a) ->
  Operator (Mega.ParsecT Void Text Identity) a
binary op f = InfixL (f <$ symbolS op)

prefix ::
  Text ->
  (a -> a) ->
  Operator (Mega.ParsecT Void Text Identity) a
prefix op f = Prefix (f <$ symbolS op)

-- match all tailing blank symbols
symbolS :: Text -> Parser Text
symbolS = symbol space

-- match tailing spaces
symbolW :: Text -> Parser Text
symbolW = symbol (void . many $ single ' ')

parens :: Parser a -> Parser a
parens = between (symbolW "(") (symbolW ")")

variableParser :: Parser Shan.Ast.Diagram.Variable
variableParser =
  ( do
      space
      nameParser
  )
    <?> "variable"

nameParser :: Parser Text
nameParser =
  ( do
      firstChar <- letterChar
      restChars <- many (alphaNumChar <|> char '_')
      void (many (single ' '))
      let v = firstChar : restChars
      if v `elem` keywords
        then fail "keywords cannot be used as variables"
        else return $ T.pack v
  )
    <?> "name"

numberParser :: Parser Double
numberParser =
  lexeme (void (many (single ' '))) (try float <|> fmap intToDouble decimal) <?> "number"
  where
    intToDouble :: Integer -> Double
    intToDouble = fromIntegral

findRelations :: [Element] -> [Relation]
findRelations =
  filter isRelation . mapMaybe toRelation
  where
    toRelation (BasicE _) = Nothing
    toRelation (RelationE r) = Just r
    isRelation r = (r ^. element . elementType) == RelationType

findBasicUnsafe :: [Element] -> UMLType -> Basic
findBasicUnsafe es t =
  let fs = filter (=? t) es
   in if null fs
        then error ("no expected element: " ++ show t)
        else
          let e = head fs
           in case e of
                BasicE b -> b
                RelationE _ -> error "impossible"

findElementSD :: [Element] -> Basic
findElementSD es = findBasicUnsafe es UMLSequenceAllInOne

findElementNote :: [Element] -> Maybe Basic
findElementNote es =
  find (=? UMLNote) es >>= elementToBasic
  where
    elementToBasic (BasicE b) = Just b
    elementToBasic (RelationE _) = Nothing

findInitialNode :: [Element] -> Basic
findInitialNode es = findBasicUnsafe es UMLSpecialState

findNodes :: [Element] -> [Basic]
findNodes =
  mapMaybe toBasic
  where
    toBasic (BasicE b) =
      if (b ^. elementType) == UMLState
        then Just b
        else Nothing
    toBasic (RelationE _) = Nothing