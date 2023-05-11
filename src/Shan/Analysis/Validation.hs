module Shan.Analysis.Validation(
  validateDiagrams
) where
import Shan.Ast.Diagram (SequenceDiagram, Automaton, Diagrams)

validateDiagrams :: Diagrams -> Either (IO ()) Diagrams
validateDiagrams = undefined