module Run where

import CommonParserUtil

import Lexer
import Terminal
import Parser
import Expr

import Control.Monad (when)
import System.IO

doProcess verbose fileName = do
  text <- readFile fileName
  -- when (verbose) $ putStrLn "Lexing..."
  -- terminalList <- lexing lexerSpec text
  when (verbose) $ putStrLn "Parsing..."
  exprSeqAst <- parsing False parserSpec ((), 1, 1, text) (aLexer lexerSpec)
  
  when (verbose) $ putStrLn "Pretty Printing..."
  when (verbose) $ putStrLn (pprintAst exprSeqAst)
  return (pprintAst exprSeqAst)
