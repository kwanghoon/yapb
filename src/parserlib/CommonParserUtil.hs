{-# LANGUAGE GADTs #-}
module CommonParserUtil
  ( LexerSpec(..), ParserSpec(..), AutomatonSpec(..),AutomatonTime(..)
  , LexerParserState, Line, Column
  , ProdRuleStr, ParseAction, ProdRulePrec
  , Stack, StkElem(..), push, pop, prStack
  , currentState, lookupGotoTable, lookupActionTable, lookupActionTableWithError
  , isReducible
  {-, toChildren -}
  , HandleParseError(..), defaultHandleParseError
  , {- matchLexSpec, -} LexAction, aLexer
  , lexing {- , lexingWithLineColumn, _lexingWithLineColumn -}
  , parsing
  {- , initState, runAutomaton -}
  , get, getText
  , LexError(..), ParseError(..), lpStateFrom
  , successfullyParsed, handleLexError, handleParseError
  , SynCompSpec(..)
  , ParserTime(..)
  )
where

import Attrs ( TokenAttrs(..), ProdRuleAttrs(..) )
import Terminal
    ( Terminal(..),
      terminalToString,
      terminalToLine,
      terminalToCol,
      terminalToSymbol,
      terminalToMaybeToken,
      tokenTextFromTerminal )
import TokenInterface ( TokenInterface(..) )

import Text.Regex.TDFA ( (=~) )
import System.Exit ( ExitCode(ExitSuccess, ExitFailure), exitWith )
import System.Process ( rawSystem )
import Control.Monad ( Monad(return), when )
import qualified Control.Monad.Trans.State.Lazy as ST

import Data.Typeable ( Typeable )
import Control.Exception ( catch, throwIO, Exception, throw )
import ParseError ( ParseError(..), lpStateFrom )

import SaveProdRules ( saveProdRules, getHashFileName )
import AutomatonType
    ( Action(Reduce, Accept, Shift),
      prProdRule,
      AutomatonTime(..),
      AutomatonSpec(..) )
import LoadAutomaton ( loadAutomaton )
import AutomatonStack
    ( Stack,
      StkElem(..),
      get,
      getText,
      emptyStack,
      push,
      pop,
      prStack )
import ParserSpec
    ( SynCompSpec(..),
      ParserTime(..),
      ParserSpec(..),
      ProdRulePrecs,
      ProdRulePrec,
      ParseAction,
      ProdRuleStr,
      TokenPrecAssoc,
      LexerSpec(..),
      RegexprActionList,
      LexAction,
      LexerParserMonad,
      LexerParserState,
      Column,
      Line )
import AutomatonUtil
    ( currentState,
      lookupActionTable,
      lookupGotoTable,
      errorKeyword,
      lookupActionTableWithError,
      revTakeRhs,
      takeRet )

import Data.List (nub)
import Data.Maybe ( Maybe(..), fromJust, isJust, isNothing )

import SynCompAlgoUtil
    ( CompCandidates(..),
      Automaton(Automaton, actTbl, gotoTbl, prodRules),
      CandidateTree(Leaf),
      Candidate(..),
      init_r_level,
      init_gs_level,
      initSearchState,
      debug,
      multiDbg,
      prlevel )
import SynCompInterface ( EmacsDataItem(..) )

import Config
    ( Configuration(config_DEBUG, config_PRESENTATION, config_COLLECT), readConfig )

import Prelude hiding (catch)
import Debug.Trace (trace)
import System.Directory ( removeFile )
import System.IO.Error hiding (catch)

import System.TimeIt (timeItT)
import Control.Monad.IO.Class(MonadIO(liftIO))
import Text.Printf ( printf )
import GenParseActions (runYapbAutomatonWithLog)

-- | Common parser utilities:
-- |
-- |  1. A parser(and lexer) specification interface to the parser generator
-- |  2. An automation executor
-- |  3. A generator for syntax completion candidates
-- | 
-- |  (TODO: Need to modularize these utilities)

-- | A Lexer and Parser Monad
-- |
-- |  - Common elements   : line, column, text
-- |  - Extended elements : a  (E.g., what the lexer and the parser want to share)
-- |  - Extended effects  : m  (E.g., typically, IO)

-- | Data structure 
-- |  (Lexer)              ===> LexerSpec         (written by applications)
-- |  (Parser)             ===> ParserSpec        (written by applications)
-- |  (Automaton)          ===> AutomatonSpec     (constructed by parsing() and used by runAutomaton)
-- |
-- |  (Automaton)          ===> ParseError        (thrown by the automaton.   NotFoundAction | NotFoundGoto
-- |                                               for the parser state on the parse error)
-- |
-- |  (Application)        ===> HandleParseError  (for options set by applications
-- |                                               to control the syntax completer)
-- |
-- |  (Syntax completer)   ===> CompCandidates    (for the syntax completer options
-- |                                               used by compCandidatesFn)

--------------------------------------------------------------------------------  
-- | The lexing machine
--------------------------------------------------------------------------------  

data LexError = LexError Int Int String  -- Line, Col, Text
  deriving (Typeable, Show)

instance Exception LexError

-- LexError line col text
--   - "No matching lexer spec at "
--   - "Line " ++ show line
--   - "Column " ++ show col
--   - " : "
--   - take 10 text

-- | Matched token = (text, optional token, spec)
type MatchedSpec  = String
type MatchedToken token = (String, Maybe token, MatchedSpec)


--------------------------------------------------------------------------------
-- | [Core] A demand-driven lexer
--------------------------------------------------------------------------------

matchLexSpec :: (Monad m, TokenInterface token) =>
  Bool -> token -> RegexprActionList token m a      -- token => End Of token!
       -> LexerParserMonad m a (MatchedToken token)

matchLexSpec debugFlag eot lexerspec =
  mls debugFlag eot lexerspec

  where
     mls
       :: (Monad m, TokenInterface token) =>
          Bool
          -> token
          -> RegexprActionList token m a
          -> ST.StateT (LexerParserState a) m (MatchedToken token)
     mls debugFlag eot lexerspec =
       do (state_parm, line, col, text) <- ST.get
          mlsSub debugFlag eot lexerspec (state_parm, line, col, text)

     mlsSub
       :: (Monad m, TokenInterface token) =>
          Bool
          -> token
          -> RegexprActionList token m a
          -> LexerParserState a
          -> ST.StateT (LexerParserState a) m (MatchedToken token)
     mlsSub debugFlag eot lexerspec (state_parm, line, col, []) =
       do ST.put (state_parm, line+1, 1, [])                  -- EOT at (line+1,1)?
          return (fromToken eot, Just eot, fromToken eot)

     mlsSub debugFlag eot [] (state_parm, line, col, text) = do
       throw (CommonParserUtil.LexError line col (takeRet 0 text))

     mlsSub debugFlag eot ((aSpec,tokenBuilder):lexerspec) (state_parm, line, col, text) = do
       let (pre, matched, post) = text =~ aSpec :: (String,String,String)
       case pre of
         "" -> let (line_, col_) = moveLineCol line col matched in
                if line==line_ && col==col_
                then
                  throw (CommonParserUtil.LexError line col ("Found RegExp for \"\"? " ++ aSpec))

                else
                  do maybeTok <- tokenBuilder matched

                     let str_maybeTok = maybe "Nothing" fromToken maybeTok

                     (state_parm_, _, _, _) <- ST.get
                     ST.put (state_parm_, line_, col_, post)

                     debug debugFlag "" $

                      debug debugFlag ("Lexer: - " ++ show aSpec ++ " " ++ matched ++ " at " ++ show (line, col)) $
                       debug debugFlag ("       - returns: " ++ if isNothing maybeTok then "Nothing" else str_maybeTok) $

                        return (matched, maybeTok, aSpec)

         _  -> mlsSub debugFlag eot lexerspec (state_parm, line, col, text)


moveLineCol :: Line -> Column -> String -> (Line, Column)
moveLineCol line col ""          = (line, col)
moveLineCol line col ('\n':text) = moveLineCol (line+1) 1 text
moveLineCol line col (ch:text)   = moveLineCol line (col+1) text


-- | Repeat matchLexSpec until all tokens are retrieved
repMatchLexSpec :: (Monad m, TokenInterface token) =>
  Bool -> token -> RegexprActionList token m a ->     -- token => End Of token!
  LexerParserMonad m a [Terminal token]
repMatchLexSpec debugFlag eot lexerspec  =
  repMLS debugFlag eot lexerspec []

  where
    repMLS debugFlag eot lexerspec terminalList =
      do (_, line, col, _) <- ST.get

         (text, maybeToken, lexSpec) <-
           matchLexSpec debugFlag eot lexerspec

         case maybeToken of
           Just token ->
             if isEOT token
             then return (reverse terminalList)
             else do repMLS debugFlag eot lexerspec
                       (Terminal text line col maybeToken : terminalList)

           Nothing    -> repMLS debugFlag eot lexerspec terminalList

-- | Getting the next token

type Lexer m a token = LexerParserMonad m a (Terminal token)
type BoolToLexer m a token = Bool -> LexerParserMonad m a (Terminal token)

aLexer
  :: (Monad m, TokenInterface token) =>
     LexerSpec token m a ->
     Bool -> LexerParserMonad m a (Terminal token)
aLexer lexerSpec =
  let eot = endOfToken lexerSpec
      regexprActionList = lexerSpecList lexerSpec

      boolToLexer flag =
        do (_, line, col, _) <- ST.get

           (matchedText, maybeTok, _) <- matchLexSpec flag eot regexprActionList

           case maybeTok of
             Nothing  -> boolToLexer flag
             Just tok -> return (Terminal matchedText line col maybeTok)

  in boolToLexer

-- | Lexing only intefaces
lexing :: (Monad m, TokenInterface token) =>
  LexerSpec token m a -> a -> String -> m [Terminal token]
lexing lexerspec state_parm text = do
  lexingWithLineColumn lexerspec state_parm 1 1 text


lexingWithLineColumn :: (Monad m, TokenInterface token) =>
  LexerSpec token m a -> a -> Line -> Column -> String -> m [Terminal token]
lexingWithLineColumn = _lexingWithLineColumn False


_lexingWithLineColumn :: (Monad m, TokenInterface token) =>
  Bool -> LexerSpec token m a -> a -> Line -> Column -> String -> m [Terminal token]
_lexingWithLineColumn debugFlag lexerspec state_parm line col text =
  do
  ST.evalStateT
      (repMatchLexSpec debugFlag (endOfToken lexerspec) (lexerSpecList lexerspec))
        (state_parm, line, col, text)


--------------------------------------------------------------------------------
-- | Automaton 
--------------------------------------------------------------------------------

initState = 0


-- Automaton

runAutomaton
  :: (TokenInterface token,
      Typeable token, Typeable ast, Typeable a,
      Show token, Show ast) =>
     Bool -> AutomatonSpec token ast IO a ->
     LexerParserState a -> BoolToLexer IO a token -> IO ast
runAutomaton flag amSpec init_lp_state lexer =
  do maybeConfig <- readConfig

     flag <- case maybeConfig of                -- ToDo: This is a dead code! 
              Nothing -> return flag
              Just config -> return $ config_DEBUG config

     collectFlag <- case maybeConfig of
                        Nothing -> return False
                        Just config -> return $ config_COLLECT config

     ST.evalStateT (runYA collectFlag) init_lp_state
     where
       runYA collectFlag =
         do start_cputime <- am_startTime (am_time amSpec)
            if not collectFlag
            then runYapbAutomaton flag amSpec{am_time=(am_time amSpec){am_cputime=start_cputime}} (lexer flag)
            else do runYapbAutomatonWithLog flag amSpec{am_time=(am_time amSpec){am_cputime=start_cputime}} (lexer flag)
                    error "This is normally terminated after collection. Don't panic!"
                    -- runYapbAutomaton flag amSpec{am_time=(am_time amSpec){am_cputime=start_cputime}} (lexer flag)  -- ToDo: always parse error!!

runYapbAutomaton
  :: (Monad m,
      TokenInterface token,
      Typeable token, Typeable ast, Typeable a,
      Show token, Show ast) =>
  -- debug flag
  Bool ->

  -- static part ActionTable,GotoTable,ProdRules,ParseActionList token ast ->
  AutomatonSpec token ast m a ->

  -- dynamic part
  ST.StateT (LexerParserState a) m (Terminal token) ->

  -- AST
  ST.StateT (LexerParserState a) m ast

runYapbAutomaton flag am_spec@AutomatonSpec {
      am_initState=initState,
      am_actionTbl=actionTbl,
      am_gotoTbl=gotoTbl,
      am_prodRules=prodRules,
      am_parseFuns=pFunList
   } nextTerminal =

      do let initStack = push (StkState initState) emptyStack
         run Nothing initStack Nothing

  where
    {- run :: TokenInterface token => [Terminal token] -> Stack token ast -> IO ast -}
    run maybeTerminal stack _maybeStatus = do
      let state = currentState stack
      -- let terminal = head terminalList

      -- Save the current state in case of going back
      prevState <- ST.get

      terminal <- maybe nextTerminal return maybeTerminal      -- Use a terminal multiple times
                                                               -- when reducing multiple production rules

      maybeStatus <-
            -- if isNothing _maybeStatus && length terminalList == 1   -- if terminal == "$"
            if isNothing _maybeStatus
               && isJust (terminalToMaybeToken terminal)
               && isEOT (fromJust (terminalToMaybeToken terminal))
            then debug flag "" $
                 debug flag ("Saving: " ++ prStack stack) $
                 do return (Just stack)
            else do return _maybeStatus

      case lookupActionTable actionTbl state terminal of
        Just action -> do
          -- putStrLn $ terminalToString terminal {- debug -}
          runAction state terminal action stack maybeStatus

        Nothing ->
          debug flag ("lookActionTable failed (1st) with: " ++ show (terminalToString terminal)) $

          case lookupActionTableWithError actionTbl state of
            Just action -> do
              -- errorTerminal intended to share the same (line,col) as terminal
              let (_, line, col, _) = prevState
              let errorTerminal     = Terminal errorKeyword line col Nothing

              -- Restore the current state
              ST.put prevState

              -- run this action (Shift toState)
              runAction state errorTerminal action stack maybeStatus

            Nothing ->
              -- No more way to proceed now!
              do lp_state <- ST.get
                 am_finishTime (am_time am_spec) (am_cputime (am_time am_spec))
                 throw (NotFoundAction terminal state stack actionTbl gotoTbl prodRules lp_state maybeStatus)

    -- separated to support the haskell layout rule
    runAction state terminal action stack maybeStatus =
      debug flag ("\nState " ++ show state) $
      debug flag ("Token " ++ tokenTextFromTerminal terminal) $
      debug flag ("Stack " ++ prStack stack) $

      case action of
        Accept ->
          debug flag "Accept" $
          debug flag (terminalToString terminal) $ {- debug -}

          case stack !! 1 of
            StkNonterminal (Just ast) _ ->
              do am_finishTime (am_time am_spec) (am_cputime (am_time am_spec))
                 return ast
            StkNonterminal Nothing _ -> error "Empty ast in the stack nonterminal"
            _ -> error "Not Stknontermianl on Accept"

        Shift toState ->
            debug flag ("Shift " ++ show toState) $
            debug flag (terminalToString terminal) $ {- debug -}

            let stack1 = push (StkTerminal terminal) stack in
            let stack2 = push (StkState toState) stack1 in
            do run Nothing stack2 maybeStatus  -- Nothing means consuming the terminal!

        Reduce n ->
            debug flag ("Reduce " ++ show n) $

            let prodrule = prodRules !! n in

            debug flag ("\t" ++ prProdRule prodrule) $

            let builderFun = pFunList  !! n in
            let lhs        = fst prodrule in
            let rhsLength  = length (snd prodrule) in
            let rhsAst = revTakeRhs rhsLength stack in
            do ast <- builderFun rhsAst
               let stack1 = drop (rhsLength*2) stack
               let topState = currentState stack1

               toState <-
                    case lookupGotoTable gotoTbl topState lhs of
                      Just state -> return state
                      Nothing -> do lp_state <- ST.get
                                    am_finishTime (am_time am_spec) (am_cputime (am_time am_spec))
                                    throw (NotFoundGoto topState lhs stack
                                             actionTbl gotoTbl prodRules
                                               lp_state maybeStatus)

               let stack2 = push (StkNonterminal (Just ast) lhs) stack1
               let stack3 = push (StkState toState) stack2
               run (Just terminal) stack3 maybeStatus -- Use the terminal again the next time!

-- prlevel :: Int -> String
-- prlevel n = take n (let spaces = ' ' : spaces in spaces)


--------------------------------------------------------------------------------
-- | Parsing interfaces
--------------------------------------------------------------------------------

parsing  :: (TokenInterface token, Typeable token, Typeable ast, Typeable a, Show token, Show ast) =>
  Bool -> ParserSpec token ast IO a -> LexerParserState a -> BoolToLexer IO a token -> String -> IO ast

parsing flag parserSpec init_lp_state lexer eot = do
  -- 1. Save production rules (in the parser spec, e.g., Parser.hs)
  --    to a grammar file.
  writtenBool <- saveProdRules specFileName sSym pSpecList tokenAttrsStr prodRuleAttrsStr eot

  -- 2. Given a grammar file,
  --    run the following command to generate prod_rules/action_table/goto_table files.
  --
  --     $ stack exec -- yapb-exe mygrammar.grm \
  --                  -output prod_rules.txt action_table.txt goto_table.txt
  --
  when writtenBool generateAutomaton

  -- 3. Load automaton files (prod_rules/action_table/goto_table.txt)
  (actionTbl, gotoTbl, prodRules) <-
    loadAutomaton grammarFileName actionTblFileName gotoTblFileName

  -- 4. Run the automaton
  if null actionTbl || null gotoTbl || null prodRules
    then do let hashFile = getHashFileName specFileName
            putStrLn $ "Delete " ++ hashFile
            removeIfExists hashFile
            error $ "Error: Empty automation: please rerun"
    else do runAutomaton flag
                     (AutomatonSpec {
                       am_initState=initState,
                       am_actionTbl=actionTbl,
                       am_gotoTbl=gotoTbl,
                       am_prodRules=prodRules,
                       am_parseFuns=pFunList,
                       am_time=AutomatonTime {
                           am_startTime=pa_startTime (parserTime parserSpec),
                           am_finishTime=pa_finishTime (parserTime parserSpec),
                           am_cputime  =0
                           }
                       })
                     init_lp_state lexer

  where
    specFileName      = parserSpecFile parserSpec  -- e.g., mygrammar.grm
    grammarFileName   = grammarFile    parserSpec  --       prod_rules.txt
    actionTblFileName = actionTblFile  parserSpec  --       action_table.txt
    gotoTblFileName   = gotoTblFile    parserSpec  --       goto_table.txt 

    sSym          = startSymbol parserSpec
    tokenAttrList = tokenPrecAssoc parserSpec

    tokenAttrs    = toTokenAttrs tokenAttrList
    tokenAttrsStr = show tokenAttrs

    synCompSpecMaybe = synCompSpec parserSpec

    pSpecList = map (\(f,s,t)->f) (parserSpecList parserSpec)
    pFunList  = map (\(f,s,t)->s) (parserSpecList parserSpec)
    pPrecList = map (\(f,s,t)->t) (parserSpecList parserSpec)

    prodRuleAttrs = toProdRuleAttrs tokenAttrs pPrecList
    prodRuleAttrsStr = show prodRuleAttrs

    generateAutomaton = do
      exitCode <- rawSystem "stack"
                  [ "exec", "--",
                    "yapb-exe", specFileName, "-output",
                    grammarFileName, actionTblFileName, gotoTblFileName
                  ]
      case exitCode of
        ExitFailure code -> exitWith exitCode
        ExitSuccess -> putStrLn ("Successfully generated: " ++
                                 actionTblFileName ++ ", "  ++
                                 gotoTblFileName ++ ", " ++
                                 grammarFileName);

-- | Utitility for parsing

toTokenAttrs :: TokenPrecAssoc -> TokenAttrs
toTokenAttrs tokenSpec = TokenAttrs (toTokenAttrs' tokenSpec 1)
  where
    toTokenAttrs' [] n = []
    toTokenAttrs' ((assoc, tokenPlaceholderList):tokenSpec) n =
      [ (tokenOrPlaceholder, (assoc, n)) | tokenOrPlaceholder <- tokenPlaceholderList ]
        ++ toTokenAttrs' tokenSpec (n+1)

toProdRuleAttrs :: TokenAttrs -> ProdRulePrecs -> ProdRuleAttrs
toProdRuleAttrs tokenAttrs prodRulePrecs = ProdRuleAttrs (toProdRuleAttrs' prodRulePrecs 0)
  where
    TokenAttrs tokenAttrs' = tokenAttrs

    toProdRuleAttrs' [] n = []

    toProdRuleAttrs' (Nothing:prodRulePrecs) n =
      toProdRuleAttrs' prodRulePrecs (n+1)

    toProdRuleAttrs' ((Just tokenOrPlaceholder):prodRulePrecs) n =
      case [ (assoc, prec)
           | (tokenOrPlaceholder', (assoc, prec)) <- tokenAttrs'
           , tokenOrPlaceholder==tokenOrPlaceholder' ] of
        [] -> error $ "The production rule #" ++ show n ++ " refers to "
                        ++ tokenOrPlaceholder ++ " but not found"
        ((assoc,prec):_) -> (n,(assoc,prec)) : toProdRuleAttrs' prodRulePrecs (n+1)


--
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

--------------------------------------------------------------------------------
-- | Computing candidates
--------------------------------------------------------------------------------

toChildren []                          = []
toChildren (StkState i:stk)            = toChildren stk
toChildren (StkTerminal term:stk)      =
  Leaf (TerminalSymbol (terminalToSymbol term)) : toChildren stk
toChildren (StkNonterminal ast nt:stk) =
  Leaf (NonterminalSymbol nt) : toChildren stk


-- --
isReducible :: TokenInterface token =>
 [(a, [String])] -> Int -> [StkElem token ast] -> Bool
isReducible productionRules prnum stk =
  let
      prodrule   = productionRules !! prnum
      rhs = snd prodrule

      rhsLength = length rhs

      prefix_stk = take (rhsLength*2) stk
      reducible =
        isMatched rhs
          (reverse [elem | (i,elem) <- zip [1..] prefix_stk, even i])
  in
    reducible

isMatched :: TokenInterface token => [String] -> [StkElem token ast] -> Bool
isMatched [] [] = True
isMatched (s:rhs) (StkTerminal terminal:stk) =
  let maybeToken = terminalToMaybeToken terminal in
    (  isJust maybeToken && s == fromToken (fromJust maybeToken) -- Todo: A bit hard-coded!!
    || isNothing maybeToken && s == terminalToSymbol terminal)
    && isMatched rhs stk
isMatched (s:rhs) (StkNonterminal _ nonterminal:stk) =
  s == nonterminal && isMatched rhs stk
isMatched _ _ = False

-- | Parsing programming interfaces

-- | successfullyParsed
successfullyParsed :: IO [EmacsDataItem]
successfullyParsed = return [SynCompInterface.SuccessfullyParsed]

-- | handleLexError
handleLexError :: IO [EmacsDataItem]
handleLexError = return [SynCompInterface.LexError]

data HandleParseError token = HandleParseError {
    debugFlag :: Bool,
    searchMaxLevel :: Int,
    simpleOrNested :: Bool,
    postTerminalList :: [Terminal token],
    nonterminalToStringMaybe :: Maybe (String->String),
    presentation :: Int,  -- 0: default, no transformation. 1: a list of the first symbols
    hpe_synCompSpec    :: Maybe SynCompSpec
  }

defaultHandleParseError lexerSpec parserSpec =
  HandleParseError {
    debugFlag = False,
    searchMaxLevel = 1,
    simpleOrNested = True,
    postTerminalList = [],
    nonterminalToStringMaybe = Nothing,
    presentation = 0,
    hpe_synCompSpec = synCompSpec parserSpec
  }

-- | handleParseError

handleParseError :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
  (  CompCandidates token ast
     -> Int
     -> [Candidate]
     -> Int
     -> Stack token ast
     -> IO ([[Candidate]], Bool) ) ->
  HandleParseError token -> ParseError token ast a -> IO [EmacsDataItem]
handleParseError compCandidatesFn hpeOption parseError =
  do maybeConfig <- readConfig

     let hpeOption' =
           case maybeConfig of
             Nothing     -> hpeOption
             Just config -> hpeOption{debugFlag = config_DEBUG config
                                      ,presentation = config_PRESENTATION config}

     let (state, stk, automaton, lp_state, maybeStatus) = infoFromParseError parseError 
     arrivedAtTheEndOfSymbol compCandidatesFn hpeOption' state stk automaton lp_state maybeStatus

    --  unwrapParseError compCandidatesFn hpeOption' parseError

infoFromParseError (NotFoundAction _ state stk _actTbl _gotoTbl _prodRules lp_state maybeStatus) = 
  let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules} in
    (state, stk, automaton, lp_state, maybeStatus)
infoFromParseError (NotFoundGoto state _ stk _actTbl _gotoTbl _prodRules lp_state maybeStatus) =
  let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules} in
    (state, stk, automaton, lp_state, maybeStatus)

--
-- unwrapParseError compCandidatesFn hpeOption (NotFoundAction _ state stk _actTbl _gotoTbl _prodRules lp_state maybeStatus) = do
--     let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules}
--     arrivedAtTheEndOfSymbol compCandidatesFn hpeOption state stk automaton lp_state maybeStatus

-- unwrapParseError compCandidatesFn hpeOption (NotFoundGoto state _ stk _actTbl _gotoTbl _prodRules lp_state maybeStatus) = do
--     let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules}
--     arrivedAtTheEndOfSymbol compCandidatesFn hpeOption state stk automaton lp_state maybeStatus

--
arrivedAtTheEndOfSymbol compCandidatesFn hpeOption state stk automaton lp_state@(_,_,_,"") maybeStatus =  -- [$]
  case maybeStatus of
    Nothing ->
      debug (debugFlag hpeOption) "No saved stack" $
      _handleParseError compCandidatesFn hpeOption state stk automaton
    Just savedStk ->
      let savedStk = fromJust maybeStatus
          savedState = currentState savedStk
      in
        debug (debugFlag hpeOption) "Restored stack" $
        debug (debugFlag hpeOption) (" - state: " ++ show savedState) $
        debug (debugFlag hpeOption) (" - stack: " ++ prStack savedStk) $
        _handleParseError compCandidatesFn hpeOption savedState savedStk automaton

arrivedAtTheEndOfSymbol compCandidatesFn hpeOption state stk automaton lp_state@(_,_,_,text) maybeStatus =
     -- debug (debugFlag hpeOption) $ "length terminalList /= 1 : " ++ show (length terminalList)
     -- debug (debugFlag hpeOption) $ map (\t -> terminalToString $ t) terminalList
     return [SynCompInterface.ParseError text]

_handleParseError
  compCandidatesFn
  hpeOption@HandleParseError {
      debugFlag=flag,
      searchMaxLevel=maxLevel,
      simpleOrNested=isSimple,
      postTerminalList=terminalListAfterCursor,
      nonterminalToStringMaybe=_nonterminalToStringMaybe,
      presentation=howtopresent,
      hpe_synCompSpec=synCompSpecMaybe}
  state stk automaton =
  let ccOption =
        CompCandidates
        {
          cc_debugFlag=flag,
          cc_printLevel=0,
          cc_maxLevel=maxLevel,

          cc_r_level = init_r_level,
          cc_gs_level = init_gs_level,

          cc_simpleOrNested=isSimple,
          cc_automaton=automaton,
          cc_searchState = initSearchState init_r_level init_gs_level,

          cc_isAbleToSearch =
            case synCompSpecMaybe of
              Nothing -> (\sym -> True)                      -- Every symbol
              Just synCompSpec -> isAbleToSearch synCompSpec -- Only those symbols that this function returns true
        }
  in
  let convFun =
        case _nonterminalToStringMaybe of
          Nothing -> const "..."
          Just fn -> fn
  in
  do (candidateListList, emacsDisplay) <- timeItShow (compCandidatesFn ccOption 0 [] state stk)
     let colorListList_symbols =
          [ filterCandidates candidateList terminalListAfterCursor
          | candidateList <- candidateListList ]

     let colorListList_ = map (stringfyCandidates convFun) colorListList_symbols
     let colorListList = map collapseCandidates colorListList_
     let emacsColorListList  = map (map showEmacsColor) colorListList
     let strList = nub [ concatStrList strList | strList <- emacsColorListList ]
     let rawStrListList = nub (map (map showRawEmacsColor) colorListList)

     debug (flag || True) "Candidates:" $
      multiDbg (map (debug (flag || True)) (map show colorListList_symbols)) $

      debug (flag || True) "" $
       multiDbg (map (debug (flag || True)) (map show rawStrListList)) $

        debug (flag || True) "" $

     -- debug (flag || True) $ showConcat $ map (\x -> (show x ++ "\n")) colorListList_symbols
     -- debug (flag || True) $ showConcat $ map (\x -> (show x ++ "\n")) rawStrListList -- mapM_ (putStrLn . show) rawStrListList

         let formattedStrList =
               case howtopresent of
                 0 -> strList
                 1 -> nub [ if null strList then "" else head strList | strList <- emacsColorListList ]
                 _ -> error ("Does not support prsentation method: " ++ show howtopresent)

         in

         if emacsDisplay
           then return (map Candidate formattedStrList)
           else return []

  where
    showConcat [] = ""
    showConcat (s:ss) = s ++ " " ++ showConcat ss

--
-- multiDbg [] = \x -> x
-- multiDbg (f:fs) = f . multiDbg fs

-- | Filter the given candidates with the following texts
data EmacsColor =
    Gray  String Line Column -- Overlapping with some in the following text
  | White String             -- Not overlapping
  deriving (Show, Eq)

-- for debugging EmacsColor in terms of symbols before they are stringfied
data EmacsColorCandidate =
    GrayCandidate  Candidate Line Column -- Overlapping with some in the following text
  | WhiteCandidate Candidate             -- Not overlapping
  deriving Eq

instance Show EmacsColorCandidate where
  showsPrec p (GrayCandidate c lin col) = (++) $ "Gray " ++ show c
  showsPrec p (WhiteCandidate c) = (++) $ "White " ++ show c

filterCandidates :: (TokenInterface token) => [Candidate] -> [Terminal token] -> [EmacsColorCandidate]
filterCandidates candidates terminalListAfterCursor =
  f candidates terminalListAfterCursor []
  where
    f (a:alpha) (b:beta) accm
      | equal a b       = f alpha beta     (GrayCandidate a (terminalToLine b) (terminalToCol b) : accm)
      | otherwise       = f alpha (b:beta) (WhiteCandidate a : accm)
    f [] beta accm      = reverse accm
    f (a:alpha) [] accm = f alpha [] (WhiteCandidate a : accm)

    equal (TerminalSymbol s1)    (Terminal s2 _ _ _) = s1==s2
    equal (NonterminalSymbol s1) _                   = False

stringfyCandidates :: (String -> String) -> [EmacsColorCandidate] -> [EmacsColor]
stringfyCandidates convFun candidates = map stringfyCandidate candidates
  where
    stringfyCandidate (GrayCandidate sym line col) = Gray (strCandidate sym) line col
    stringfyCandidate (WhiteCandidate sym) = White (strCandidate sym)

    strCandidate (TerminalSymbol s) = s
    strCandidate (NonterminalSymbol s) = convFun s -- "..." -- ++ s ++ "..."

collapseCandidates [] = []
collapseCandidates [a] = [a]
collapseCandidates ((Gray "..." l1 c1) : (Gray "..." l2 c2) : cs) =
  collapseCandidates ((Gray "..." l2 c2) : cs)
collapseCandidates ((White "...") : (White "...") : cs) =
  collapseCandidates ((White "...") : cs)
collapseCandidates (a:b:cs) = a : collapseCandidates (b:cs)

-- | Utilities
showSymbol (TerminalSymbol s) = s
showSymbol (NonterminalSymbol _) = "..."

showRawSymbol (TerminalSymbol s) = s
showRawSymbol (NonterminalSymbol s) = s

showEmacsColor (Gray s line col) = "gray " ++ s ++ " " ++ show line ++ " " ++ show col ++ " "
showEmacsColor (White s)         = "white " ++ s

showRawEmacsColor (Gray s line col) = s ++ "@" ++ show line ++ "," ++ show col ++ " "
showRawEmacsColor (White s)         = s

concatStrList [] = "" -- error "The empty candidate?"
concatStrList [str] = str
concatStrList (str:strs) = str ++ " " ++ concatStrList strs

-- Q. Can we make it be typed???
--
-- computeCandWith :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast)
--     => LexerSpec token -> ParserSpec token ast
--     -> String -> Bool -> Int -> IO [EmacsDataItem]
-- computeCandWith lexerSpec parserSpec str isSimple cursorPos = ((do
--   terminalList <- lexing lexerSpec str 
--   ast <- parsing parserSpec terminalList 
--   successfullyParsed)
--   `catch` \e -> case e :: LexError of _ -> handleLexError
--   `catch` \e -> case e :: ParseError token ast of _ -> handleParseError isSimple e)    

-- Utilities
-- debug :: Bool -> String -> a -> a
-- debug flag msg x = if flag then trace msg x else x

--
timeItShow :: (MonadIO m, Show a) => m a -> m a
timeItShow ioa =
  do (t,a) <- timeItT ioa
     liftIO $ printf "Time: %6.2f\n" t
     return a
