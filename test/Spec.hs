import System.Process

main :: IO ()
main = do
  text <- readProcess "test.sh" [] ""
  putStrLn text
  return ()
  
