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
import SynCompAlgorithm

-- Todo: The following part should be moved to the library.
--       Arguments: lexerSpec, parserSpec
--                  isSimpleMode
--                  programTextUptoCursor, programTextAfterCursor

maxLevel = 10000

-- | computeCand
computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode =

  (do 
      {- 1. Parsing -}
      ((do ast <- parsing debug
                    parserSpec ((),1,1,programTextUptoCursor)
                      (aLexer lexerSpec) (fromToken (endOfToken lexerSpec))
           successfullyParsed)

        `catch` \parseError ->
          case parseError :: ParseError Token AST () of
            _ ->
              {- 2. Computing candidates with it -}
              do let (_,line,column,programTextAfterCursor) = lpStateFrom parseError
                 compCandidates <- chooseCompCandidatesFn
  
                 handleParseError compCandidates
                   (defaultHandleParseError lexerSpec parserSpec)
                    {
                       debugFlag=debug,
                       searchMaxLevel=maxLevel,
                       simpleOrNested=isSimpleMode,
                       postTerminalList=[],  -- terminalListAfterCursor is set to []!
                       nonterminalToStringMaybe=Nothing
                    }
                   parseError))

      `catch` \lexError ->  case lexError :: LexError of  _ -> handleLexError
