module Shan.Parser
  ( parseShan,
  )
where

import Shan.Ast.Diagram (Automaton, SequenceDiagram)
import Shan.Ast.Diagram.Parser (parseDiagram)
import Shan.Uxf.Uxf (parseUxfFolder)

parseShan :: FilePath -> IO [Either SequenceDiagram Automaton]
parseShan p =
  parseDiagram <$$> parseUxfFolder p
  where
    (<$$>) = fmap . fmap