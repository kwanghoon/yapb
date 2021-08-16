module SyntaxCompletion (computeCand) where

import CommonParserUtil 

import TokenInterface
import Terminal
import Lexer (lexerSpec)
import Parser (parserSpec)
import System.IO

-- for syntax completion
import Token
import Expr
import SynCompInterface
import Control.Exception
import Data.Typeable

-- Todo: The following part should be moved to the library.
--       Arguments: lexerSpec, parserSpec
--                  isSimpleMode
--                  programTextUptoCursor, programTextAfterCursor

-- | computeCand
computeCand :: String -> String -> Bool -> IO [EmacsDataItem]
computeCand programTextUptoCursor programTextAfterCursor isSimpleMode = (do
  {- 1. Lexing  -}                                                                         
  (line, column, terminalListUptoCursor)  <- lexing_ lexerSpec 1 1 programTextUptoCursor

  {- 2. Parsing -}
  ((do ast <- parsing parserSpec terminalListUptoCursor
       successfullyParsed)

    `catch` \parseError ->
      case parseError :: ParseError Token AST of
        _ ->
          {- 3. Lexing the rest and computing candidates with it -}
          do (_, _, terminalListAfterCursor) <- lexing_ lexerSpec line column programTextAfterCursor
             handleParseError isSimpleMode terminalListAfterCursor parseError))

  `catch` \lexError ->  case lexError :: LexError of  _ -> handleLexError
