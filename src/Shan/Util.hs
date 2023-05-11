module Shan.Util (
  Case(..)
) where
  
import Shan.Ast.Diagram (Bound)

data Case = Case
  { name :: String,
    path :: FilePath,
    bound :: Bound
  }