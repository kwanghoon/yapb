module SyntaxCompletion (computeCand) where

import CommonParserUtil
    ( aLexer,
      defaultHandleParseError,
      handleLexError,
      handleParseError,
      parsing,
      successfullyParsed,
      lpStateFrom,
      HandleParseError(debugFlag, searchMaxLevel, simpleOrNested,
                       postTerminalList, nonterminalToStringMaybe),
      LexError,
      ParseError,
      LexerSpec(endOfToken) )

import TokenInterface ( TokenInterface(fromToken) )
import Terminal ()
import Lexer (lexerSpec)
import Parser (parserSpec)
import System.IO ()

-- for syntax completion
import Token ( Token )
import Expr (AST)
import SynCompInterface ( EmacsDataItem, stateEmacsDataItem )
import Control.Exception ( catch )
import Data.Maybe ()
import SynCompAlgorithm ( chooseCompCandidatesFn )
import Config ( readConfig, Configuration(config_TABSTATE) )

-- Todo: The following part should be moved to the library.
--       Arguments: lexerSpec, parserSpec
--                  isSimpleMode
--                  programTextUptoCursor, programTextAfterCursor

maxLevel = 10000

-- | computeCand
computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode =

  (do
      collectionMode <- isCollectionMode 

      {- 1. Parsing -}
      ((do ast <- parsing debug
                    parserSpec ((),1,1,programTextUptoCursor)
                      (aLexer lexerSpec) (fromToken (endOfToken lexerSpec))
           successfullyParsed)

        `catch` \parseError ->
          case parseError :: ParseError Token AST () of
            _ ->

              

              {- 2. Computing candidates with it -}
              do let ((_,line,column,programTextAfterCursor), stateAtTapPosition) = lpStateFrom parseError
                 if collectionMode 
                  then return $ [ stateEmacsDataItem stateAtTapPosition ]
                  else 
                    do  compCandidates <- chooseCompCandidatesFn

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

isCollectionMode :: IO Bool
isCollectionMode = maybe False config_TABSTATE <$> readConfig