module Main where

import CommonParserUtil

import Lexer
import Terminal
import Parser
import Expr

import Run(doProcess)
import ParserSpec (spec)

import System.IO
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  args <- getArgs
  _main args

-- Todo: Can I fix to have "test" as a command in stack exec?

_main [] = return ()
_main (fileName:args) = 
  case fileName of
    "test" -> withArgs [] spec
    _ -> do _ <- doProcess True fileName
            _main args

readline msg = do
  putStr msg
  hFlush stdout
  readline'

readline' = do
  ch <- getChar
  if ch == '\n' then
    return ""
  else
    do line <- readline'
       return (ch:line)


