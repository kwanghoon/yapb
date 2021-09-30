import System.Process

main :: IO ()
main = do
  text <- readProcess "./test/test.sh" [] ""
  putStrLn text
  return ()
  
