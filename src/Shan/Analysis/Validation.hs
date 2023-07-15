module Shan.Analysis.Validation
  ( validateDiagrams,
  )
where

import Data.Set ((\\))
import Data.Set qualified as S
import Shan.Ast.Diagram (Automaton, Diagrams, SequenceDiagram (SequenceDiagram), judgementVars, judgements, messages, mname)
import Text.Printf (printf)

validateDiagrams :: Diagrams -> Either String Diagrams
validateDiagrams (sds, automata) = do
  vsds <- validateSds sds
  vautomata <- validateAutomata automata
  return (vsds, vautomata)

validateSds :: [SequenceDiagram] -> Either String [SequenceDiagram]
validateSds = mapM validateSd

validateSd :: SequenceDiagram -> Either String SequenceDiagram
validateSd sd = do
  v1 <- messageNameNoCollision sd
  checkMessageNamesInConstraints v1

-- all the method names declared in the sequence diagram should not collide
messageNameNoCollision :: SequenceDiagram -> Either String SequenceDiagram
messageNameNoCollision sd@(SequenceDiagram name _ _ _) =
  let nameInSd = mname <$> messages sd
      nameSet = S.fromList nameInSd
   in if length nameInSd == S.size nameSet
        then Right sd
        else Left (printf "There is name collision in sequence diagram \"%s\"" name)

-- all the message names in the constraints are declared in the sequence diagram
checkMessageNamesInConstraints :: SequenceDiagram -> Either String SequenceDiagram
checkMessageNamesInConstraints sd@(SequenceDiagram name _ _ _) =
  let nameSet = S.fromList (mname <$> messages sd)
      nameInJudges = S.unions (judgementVars <$> judgements sd)
   in if nameInJudges `S.isSubsetOf` nameSet
        then Right sd
        else
          let undeclared = nameInJudges \\ nameSet
           in Left (printf "In sequence diagram \"%s\", the following name in constraints are not declared: %s." name (show undeclared))

-- TODO: validate automata
validateAutomata :: [Automaton] -> Either String [Automaton]
validateAutomata = Right