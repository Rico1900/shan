module Task.Shannon where


data Case = Case
  { name :: String,
    path :: FilePath
  }

printElementNames :: Case -> IO ()
printElementNames c = 