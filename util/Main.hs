------------------------------------------------------------------------------------------
-- | Filter out smallbasic programs with lexical errors or parsing errors.
------------------------------------------------------------------------------------------

-- ./smallbasic-list/TGQ394.sb
-- ./smallbasic-list/MBT602.sb
-- sbparser-exe: LexError 6 29 "\"yellow"
-- ./smallbasic-list/WGX265.sb
-- sbparser-exe: NotFoundAction: State 50 : . at (13, 18): . Line 13 Column 19 : 1,gh*.15)
-- ./smallbasic-list/RZR742.sb

-- ==>

-- ./smallbasic-list/TGQ394.sb
------------------------------------------------------------------------------------------

module Main where

import System.Environment (getArgs)

prefix     = "./smallbasic-list"
lexerror   = "sbparser-exe: LexError"
parseerror = "sbparser-exe: NotFoundAction"

main :: IO ()
main = do
  args <- getArgs
  if null args
  then putStrLn "run with a file name"
  else do text <- readFile (head args)
          mapM_ putStrLn ( reverse ( filterOut (lines text) [] ) )


filterOut [] sbProgs = sbProgs

filterOut (line:theRest) sbProgs =
  if take (length prefix) line == prefix then
    filterOut theRest (line : sbProgs)
  else if take (length lexerror) line == lexerror || take (length parseerror) line == parseerror then
    filterOut theRest (if null sbProgs then sbProgs else tail sbProgs)
  else error ( "Unexpected: " ++ line )

