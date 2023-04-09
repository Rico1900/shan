module Shan.Parser(
  parseShan
) where

import Shan.AST.Diagram (SequenceDiagram, Automaton)
import Shan.AST.Diagram.Parser (parseDiagram)
import Shan.UXF.Uxf (parseUxfFolder)

parseShan :: FilePath -> IO [Either SequenceDiagram Automaton]
parseShan p = 
  parseDiagram <$$> parseUxfFolder p
  where
    (<$$>) = fmap . fmap