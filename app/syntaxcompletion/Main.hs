module Main where

import CommonParserUtil

import Token
import Lexer
import Terminal
import Parser
import SynCompInterface
import EmacsServer
import System.IO

import Data.Typeable
import Control.Exception
import Data.List (nub)

main :: IO ()
main = do
  emacsServer computeCand
  
  -- text <- readline "Enter text to parse: "
  -- doProcess text

-- Computing candidates for syntax completion

computeCand :: String -> Int -> IO [EmacsDataItem]
computeCand str cursorPos = ((do
  terminalList <- lexing lexerSpec str 
  ast <- parsing parserSpec terminalList 
  return [SuccessfullyParsed])
  `catch` \e -> case e :: LexError of _ -> return [SynCompInterface.LexError])
  `catch` \e -> case e :: ParseError Token AST of
                  NotFoundAction _ state stk actTbl gotoTbl prodRules pFunList terminalList ->
                    if length terminalList  == 1 then do -- [$]
                      candidates <- compCandidates [] state actTbl gotoTbl prodRules pFunList stk -- return ["candidates"]
                      let cands = candidates
                      let strs = nub [ concatStrList strList | strList <- map (map showSymbol) cands ]
                      -- mapM_ putStr strs
                      return $ map Candidate strs
                    else
                      return [SynCompInterface.ParseError (map terminalToString terminalList)]
                  NotFoundGoto state _ stk actTbl gotoTbl prodRules pFunList terminalList ->
                    if length terminalList == 1 then do -- [$]
                      candidates <- compCandidates [] state actTbl gotoTbl prodRules pFunList stk
                      let cands = candidates
                      let strs = nub [ concatStrList strList | strList <- map (map showSymbol) cands ]
                      -- mapM_ putStr strs
                      return $ map Candidate strs
                    else
                      return [SynCompInterface.ParseError (map terminalToString terminalList)]
                      
showSymbol (TerminalSymbol s) = s
showSymbol (NonterminalSymbol _) = "..."

concatStrList [] = "" -- error "The empty candidate?"
concatStrList [str] = str
concatStrList (str:strs) = str ++ " " ++ concatStrList strs

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


