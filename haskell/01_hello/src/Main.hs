import System.Environment

main = do
  args <- getArgs
  putStrLn $ unlines args
