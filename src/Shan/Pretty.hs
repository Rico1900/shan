module Shan.Pretty(
  banner,
  line,
  separationLine
) where

banner :: String -> IO ()
banner info = do
  let l = length info
  line l
  putStrLn info
  line l

line :: Int -> IO ()
line n = putStrLn $ replicate n '-'

separationLine :: IO ()
separationLine = putStrLn $ replicate 35 '-'