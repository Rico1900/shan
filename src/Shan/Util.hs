module Shan.Util (
  Case(..)
) where
  
import Shan.AST.Diagram (Bound)

data Case = Case
  { name :: String,
    path :: FilePath,
    bound :: Bound
  }