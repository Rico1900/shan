module Shan.Util (
  Case(..)
) where


data Case = Case
  { name :: String,
    path :: FilePath
  }