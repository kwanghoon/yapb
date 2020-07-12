module Main where

import CommonParserUtil

import Lexer
import Terminal
import Parser
import System.IO

-- for syntax completion
import Token
import EmacsServer
import SynCompInterface
import Control.Exception

main :: IO ()
main = do
  emacsServer computeCand
  
computeCand :: String -> Bool -> Int -> IO [EmacsDataItem]
computeCand str isSimple cursorPos = ((do
  terminalList <- lexing lexerSpec str 
  ast <- parsing parserSpec terminalList 
  successfullyParsed)
  `catch` \e -> case e :: LexError of _ -> handleLexError
  `catch` \e -> case e :: ParseError Token AST of _ -> handleParseError isSimple e)


-- The normal parser
doProcess text = do
  putStrLn "Lexing..."
  terminalList <- lexing lexerSpec text
  mapM_ (putStrLn . terminalToString) terminalList
  putStrLn "Parsing..."
  exprSeqAst <- parsing parserSpec terminalList
  putStrLn "Pretty Printing..."
  putStrLn (show exprSeqAst)
  
  
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


