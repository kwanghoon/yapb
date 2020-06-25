module Main where

import CommonParserUtil

import Token
import Lexer
import Terminal
import Parser
import EmacsServer
import System.IO

import Data.Typeable
import Control.Exception

main :: IO ()
main = do
  emacsServer computeCand
  
  -- text <- readline "Enter text to parse: "
  -- doProcess text

-- Computing candidates for syntax completion

computeCand :: String -> Int -> IO [String]
computeCand str cursorPos = ((do
  terminalList <- lexing lexerSpec str 
  ast <- parsing parserSpec terminalList
  putStrLn "successfully parsed"
  return ["SuccessfullyParsed"])
  `catch` \e ->
     case e :: LexError of
       _ -> do
         putStrLn "lex error"
         return ["LexError"])
  `catch` \e ->
     case e :: ParseError Token AST of
       NotFoundAction _ state _ actTbl gotoTbl -> do
         candidates <- compCandidates [] state actTbl gotoTbl -- return ["candidates"]
         putStrLn (show candidates)
         return (map candidateToStr candidates)
       NotFoundGoto state _ _ actTbl gotoTbl -> do
         candidates <- compCandidates [] state actTbl gotoTbl
         putStrLn (show candidates)
         return (map candidateToStr candidates)

candidateToStr [] = ""
candidateToStr (TerminalSymbol s:cands)    = s ++ candidateToStr cands
candidateToStr (NonterminalSymbol _:cands) = "..." ++ candidateToStr cands


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


