module Main where

import CommonParserUtil

import Lexer
import Terminal
import Parser
import Expr

import System.IO

main :: IO ()
main = do
  fileName <- readline "Enter your file: "
  case fileName of
    "exit" -> return ()
    line -> doProcess line

doProcess line = do
  text <- readFile line 
  putStrLn "Lexing..."
  terminalList <- lexing lexerSpec text
  putStrLn "Parsing..."
  exprSeqAst <- parsing parserSpec terminalList
  putStrLn "Pretty Printing..."
  putStrLn (pprintAst exprSeqAst)
  
  
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


