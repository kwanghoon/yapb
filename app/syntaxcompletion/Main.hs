module Main where

import CommonParserUtil 

import Lexer
import Terminal
import Parser
import System.IO

-- for syntax completion
import Token
import Expr
import EmacsServer
import SynCompInterface
import Control.Exception

main :: IO ()
main = do
  emacsServer computeCand
  
computeCand :: String -> Bool -> IO [EmacsDataItem]
computeCand programTextUptoCursor isSimpleMode = ((do
  terminalList <- lexing lexerSpec programTextUptoCursor 
  ast <- parsing parserSpec terminalList 
  successfullyParsed)
  `catch` \e -> case e :: LexError of _ -> handleLexError)
  `catch` \e -> case e :: ParseError Token AST of _ -> handleParseError isSimpleMode e


