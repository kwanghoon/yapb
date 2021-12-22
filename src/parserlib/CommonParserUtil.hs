{-# LANGUAGE GADTs #-}
module CommonParserUtil
  ( LexerSpec(..), ParserSpec(..), AutomatonSpec(..), HandleParseError(..)
  , lexing, lexingWithLineColumn, _lexingWithLineColumn
  , parsing, runAutomaton, parsingHaskell, runAutomatonHaskell
  , get, getText
  , LexError(..), ParseError(..)
  , successfullyParsed, handleLexError, handleParseError) where

import Terminal
import TokenInterface

import Text.Regex.TDFA
import System.Exit
import System.Process
import Control.Monad

import Data.Typeable
import Control.Exception

import SaveProdRules
import AutomatonType
import LoadAutomaton

import Data.List (nub)
import Data.Maybe

import SynCompInterface

import Config

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)


-- Lexer Specification
type RegExpStr    = String
type LexFun token = String -> Maybe token 

type LexerSpecList token  = [(RegExpStr, LexFun token)]
data LexerSpec token =
  LexerSpec { endOfToken    :: token,
              lexerSpecList :: LexerSpecList token
            }

-- Parser Specification
type ProdRuleStr = String
type ParseFun token ast = Stack token ast -> ast

type ParserSpecList token ast = [(ProdRuleStr, ParseFun token ast)]
data ParserSpec token ast =
  ParserSpec { startSymbol    :: String,
               parserSpecList :: ParserSpecList token ast,
               baseDir        :: String,   -- ex) ./
               actionTblFile  :: String,   -- ex) actiontable.txt
               gotoTblFile    :: String,   -- ex) gototable.txt
               grammarFile    :: String,   -- ex) grammar.txt
               parserSpecFile :: String,   -- ex) mygrammar.grm
               genparserexe   :: String    -- ex) genlrparse-exe
             }

-- Specification
data Spec token ast =
  Spec (LexerSpec token) (ParserSpec token ast)

--------------------------------------------------------------------------------  
-- The lexing machine
--------------------------------------------------------------------------------  
type Line = Int
type Column = Int

--
data LexError = LexError Int Int String  -- Line, Col, Text
  deriving (Typeable, Show)

instance Exception LexError

-- prLexError (CommonParserUtil.LexError line col text) = do
--   putStr $ "No matching lexer spec at "
--   putStr $ "Line " ++ show line
--   putStr $ "Column " ++ show col
--   putStr $ " : "
--   putStr $ take 10 text

--
lexing :: TokenInterface token =>
          LexerSpec token -> String -> IO [Terminal token]
lexing lexerspec text = do
  (line, col, terminalList) <- lexingWithLineColumn lexerspec 1 1 text
  return terminalList

lexingWithLineColumn :: TokenInterface token =>
           LexerSpec token -> Line -> Column -> String -> IO (Line, Column, [Terminal token])
lexingWithLineColumn lexerspec line col text =
  _lexingWithLineColumn False lexerspec line col text

_lexingWithLineColumn debug lexerspec line col [] = do
  let eot = endOfToken lexerspec 
  return (line, col, [Terminal (fromToken eot) line col (Just eot)])

_lexingWithLineColumn debug lexerspec line col text = do  --Todo: make it tail-recursive!
  (matchedText, theRestText, maybeTok, aSpec) <-
    matchLexSpec line col (lexerSpecList lexerspec) text
  let (line_, col_) = moveLineCol line col matchedText

  let str_maybeTok = if isNothing maybeTok then "Nothing" else (fromToken (fromJust maybeTok))
  
  when (line==line_ && col==col_) $
    throw (CommonParserUtil.LexError line col ("length-zero regexp detected? " ++ aSpec))
  
  when (debug) $ putStrLn $ show (line_, col_)
  when (debug) $ putStrLn $ matchedText
  when (debug) $ putStrLn $ if isNothing maybeTok then "Nothing" else str_maybeTok
  
  (line__, col__, terminalList) <- _lexingWithLineColumn debug lexerspec line_ col_ theRestText
  case maybeTok of
    Nothing  -> return (line__, col__, terminalList)
    Just tok -> do
      let terminal = Terminal matchedText line col (Just tok)
      return (line__, col__, terminal:terminalList)

matchLexSpec :: TokenInterface token =>
                Line -> Column -> LexerSpecList token -> String
             -> IO (String, String, Maybe token, String)
matchLexSpec line col [] text = do
  throw (CommonParserUtil.LexError line col text)
  -- putStr $ "No matching lexer spec at "
  -- putStr $ "Line " ++ show line
  -- putStr $ "Column " ++ show col
  -- putStr $ " : "
  -- putStr $ take 10 text
  -- exitWith (ExitFailure (-1))

matchLexSpec line col ((aSpec,tokenBuilder):lexerspec) text = do
  let (pre, matched, post) = text =~ aSpec :: (String,String,String)
  case pre of
    "" -> return (matched, post, tokenBuilder matched, aSpec)
    _  -> matchLexSpec line col lexerspec text


moveLineCol :: Line -> Column -> String -> (Line, Column)
moveLineCol line col ""          = (line, col)
moveLineCol line col ('\n':text) = moveLineCol (line+1) 1 text
moveLineCol line col (ch:text)   = moveLineCol line (col+1) text
  
--------------------------------------------------------------------------------  
-- The parsing machine
--------------------------------------------------------------------------------

type CurrentState    = Int
type StateOnStackTop = Int
type LhsSymbol = String

type AutomatonSnapshot token ast =   -- TODO: Refactoring
  (Stack token ast, ActionTable, GotoTable, ProdRules)

--
data ParseError token ast where
    -- teminal, state, stack actiontbl, gototbl
    NotFoundAction :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
      (Terminal token) -> CurrentState -> (Stack token ast) -> ActionTable -> GotoTable -> ProdRules -> [Terminal token] ->
      Maybe [StkElem token ast] ->
      ParseError token ast
    
    -- topState, lhs, stack, actiontbl, gototbl,
    NotFoundGoto :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
      StateOnStackTop -> LhsSymbol -> (Stack token ast) -> ActionTable -> GotoTable -> ProdRules -> [Terminal token] ->
      Maybe [StkElem token ast] ->
      ParseError token ast

  deriving (Typeable)

instance (Show token, Show ast) => Show (ParseError token ast) where
  showsPrec p (NotFoundAction terminal state stack _ _ _ _ _) =
    (++) "NotFoundAction: " . (++) (show state) . (++) " " . (++) (terminalToString terminal) -- (++) (show $ length stack)
  showsPrec p (NotFoundGoto topstate lhs stack _ _ _ _ _) =
    (++) "NotFoundGoto: " . (++) (show topstate) . (++) " " . (++) lhs -- . (++) (show stack)

instance (TokenInterface token, Typeable token, Show token, Typeable ast, Show ast)
  => Exception (ParseError token ast)

--
parsing flag parserSpec terminalList =
  parsingHaskell flag parserSpec terminalList Nothing
  
parsingHaskell :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
           Bool -> ParserSpec token ast -> [Terminal token] -> Maybe token -> IO ast
parsingHaskell flag parserSpec terminalList haskellOption = do
  -- 1. Save the production rules in the parser spec (Parser.hs).
  writtenBool <- saveProdRules specFileName sSym pSpecList

  -- 2. If the grammar file is written,
  --    run the following command to generate prod_rules/action_table/goto_table files.
  --     stack exec -- yapb-exe mygrammar.grm -output prod_rules.txt action_table.txt goto_table.txt
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
    else do ast <- runAutomatonHaskell flag
                     (AutomatonSpec {
                       am_initState=initState,
                       am_actionTbl=actionTbl,
                       am_gotoTbl=gotoTbl,
                       am_prodRules=prodRules,
                       am_parseFuns=pFunList })
                     terminalList haskellOption
            -- putStrLn "done." -- It was for the interafce with Java-version RPC calculus interpreter.
            return ast

  where
    specFileName      = parserSpecFile parserSpec
    grammarFileName   = grammarFile    parserSpec
    actionTblFileName = actionTblFile  parserSpec
    gotoTblFileName   = gotoTblFile    parserSpec
    
    sSym      = startSymbol parserSpec
    pSpecList = map fst (parserSpecList parserSpec)
    pFunList  = map snd (parserSpecList parserSpec)

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
--
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- Stack

data StkElem token ast =
    StkState Int
  | StkTerminal (Terminal token)
  | StkNonterminal (Maybe ast) String -- String for printing Nonterminal instead of ast

instance TokenInterface token => Eq (StkElem token ast) where
  (StkState i)          == (StkState j)          = i == j
  (StkTerminal termi)   == (StkTerminal termj)   = tokenTextFromTerminal termi == tokenTextFromTerminal termj
  (StkNonterminal _ si) == (StkNonterminal _ sj) = si == sj

type Stack token ast = [StkElem token ast]

emptyStack = []

get :: Stack token ast -> Int -> ast
get stack i =
  case stack !! (i-1) of
    StkNonterminal (Just ast) _ -> ast
    StkNonterminal Nothing _ -> error $ "get: empty ast in the nonterminal at stack"
    _ -> error $ "get: out of bound: " ++ show i

getText :: Stack token ast -> Int -> String
getText stack i = 
  case stack !! (i-1) of
    StkTerminal (Terminal text _ _ _) -> text
    _ -> error $ "getText: out of bound: " ++ show i

push :: a -> [a] -> [a]
push elem stack = elem:stack

pop :: [a] -> (a, [a])
pop (elem:stack) = (elem, stack)
pop []           = error "Attempt to pop from the empty stack"

prStack :: TokenInterface token => Stack token ast -> String
prStack [] = "STACK END"
prStack (StkState i : stack) = "S" ++ show i ++ " : " ++ prStack stack
prStack (StkTerminal (Terminal text _ _ (Just token)) : stack) =
  let str_token = fromToken token in
  (if str_token == text then str_token else (fromToken token ++ " i.e. " ++ text))
    ++  " : " ++ prStack stack
prStack (StkTerminal (Terminal text _ _ Nothing) : stack) =
  (token_na ++ " " ++ text) ++  " : " ++ prStack stack
prStack (StkNonterminal _ str : stack) = str ++ " : " ++ prStack stack

-- Utility for Automation
currentState :: Stack token ast -> Int
currentState (StkState i : stack) = i
currentState _                    = error "No state found in the stack top"

tokenTextFromTerminal :: TokenInterface token => Terminal token -> String
tokenTextFromTerminal (Terminal _ _ _ (Just token)) = fromToken token
tokenTextFromTerminal (Terminal _ _ _ Nothing) = token_na

lookupActionTable :: TokenInterface token => ActionTable -> Int -> (Terminal token) -> Maybe Action
lookupActionTable actionTbl state terminal =
  lookupTable actionTbl (state,tokenTextFromTerminal terminal)
     ("Not found in the action table: " ++ terminalToString terminal) 

lookupGotoTable :: GotoTable -> Int -> String -> Maybe Int
lookupGotoTable gotoTbl state nonterminalStr =
  lookupTable gotoTbl (state,nonterminalStr)
     ("Not found in the goto table: ")

lookupTable :: (Eq a, Show a) => [(a,b)] -> a -> String -> Maybe b
lookupTable tbl key msg =   
  case [ val | (key', val) <- tbl, key==key' ] of
    [] -> Nothing -- error $ msg ++ " : " ++ show key
    (h:_) -> Just h


-- Note: take 1th, 3rd, 5th, ... of 2*len elements from stack and reverse it!
-- example) revTakeRhs 2 [a1,a2,a3,a4,a5,a6,...]
--          = [a4, a2]
revTakeRhs :: Int -> [a] -> [a]
revTakeRhs 0 stack = []
revTakeRhs n (_:nt:stack) = revTakeRhs (n-1) stack ++ [nt]

-- Automaton

data AutomatonSpec token ast =
  AutomatonSpec {
    am_actionTbl :: ActionTable,
    am_gotoTbl :: GotoTable,
    am_prodRules :: ProdRules,
    am_parseFuns :: ParseFunList token ast,
    am_initState :: Int
  }

initState = 0

type ParseFunList token ast = [ParseFun token ast]

runAutomaton flag amSpec terminalList =
  runAutomatonHaskell flag amSpec terminalList Nothing

runAutomatonHaskell flag amSpec terminalList haskellOption =
  do maybeConfig <- readConfig
  
     flag <- case maybeConfig of
               Nothing -> return flag
               Just config -> return $ config_DEBUG config
               
     runYapbAutomaton flag amSpec terminalList Nothing
  
runYapbAutomaton :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
  Bool -> 
  {- static part ActionTable -> GotoTable -> ProdRules -> ParseFunList token ast -> -}
  AutomatonSpec token ast -> 
  {- dynamic part -}
  [Terminal token] ->
  {- haskell parser specific option -}
  Maybe token ->
  {- AST -}
  IO ast
runYapbAutomaton flag (rm_spec @ AutomatonSpec {
      am_initState=initState,
      am_actionTbl=actionTbl,
      am_gotoTbl=gotoTbl,
      am_prodRules=prodRules,
      am_parseFuns=pFunList
   }) terminalList haskellOption = do
  let initStack = push (StkState initState) emptyStack
  run terminalList initStack Nothing
  
  where
    {- run :: TokenInterface token => [Terminal token] -> Stack token ast -> IO ast -}
    run terminalList stack _maybeStatus = do
      let state = currentState stack
      let terminal = head terminalList
      
      maybeStatus <- 
            if isNothing _maybeStatus && length terminalList == 1   -- if terminal == "$"
            then do debug flag $ ""
                    debug flag $ "Saving: " ++ (prStack stack)
                    return (Just stack)
            else do return _maybeStatus
            
      case lookupActionTable actionTbl state terminal of
        Just action -> do
          -- putStrLn $ terminalToString terminal {- debug -}
          runAction state terminal action terminalList stack maybeStatus
          
        Nothing -> do
          debug flag $ "lookActionTable failed (1st) with: " ++ show (terminalToString terminal)
          case haskellOption of
            Just extraToken -> do
              let terminal_close_brace = Terminal
                                          (fromToken extraToken)
                                            (terminalToLine terminal)
                                              (terminalToCol terminal)
                                                (Just extraToken)
              case lookupActionTable actionTbl state terminal_close_brace of
                Just action -> do
                  -- putStrLn $ terminalToString terminal_close_brace {- debug -}
                  debug flag $ "lookActionTable succeeded (2nd) with: " ++ terminalToString terminal_close_brace
                  runAction state terminal_close_brace action (terminal_close_brace : terminalList) stack maybeStatus
                  
                Nothing -> do
                  debug flag $ "lookActionTable failed (2nd) with: " ++ terminalToString terminal_close_brace
                  throw (NotFoundAction terminal state stack actionTbl gotoTbl prodRules terminalList maybeStatus)
                           
            Nothing -> throw (NotFoundAction terminal state stack actionTbl gotoTbl prodRules terminalList maybeStatus)

    -- separated to support the haskell layout rule
    runAction state terminal action terminalList stack maybeStatus = do      
      debug flag ("\nState " ++ show state)
      debug flag ("Token " ++ tokenTextFromTerminal terminal)
      debug flag ("Stack " ++ prStack stack)
      
      case action of
        Accept -> do
          debug flag "Accept"
          debug flag $ terminalToString terminal {- debug -}
          
          case stack !! 1 of
            StkNonterminal (Just ast) _ -> return ast
            StkNonterminal Nothing _ -> fail "Empty ast in the stack nonterminal"
            _ -> fail "Not Stknontermianl on Accept"
        
        Shift toState -> do
          debug flag ("Shift " ++ show toState)
          debug flag $ terminalToString terminal {- debug -}
          
          let stack1 = push (StkTerminal (head terminalList)) stack
          let stack2 = push (StkState toState) stack1
          run (tail terminalList) stack2 maybeStatus
          
        Reduce n -> do
          debug flag ("Reduce " ++ show n)
          
          let prodrule   = prodRules !! n
          
          debug flag ("\t" ++ show prodrule)
          
          let builderFun = pFunList  !! n
          let lhs        = fst prodrule
          let rhsLength  = length (snd prodrule)
          let rhsAst = revTakeRhs rhsLength stack
          let ast = builderFun rhsAst
          let stack1 = drop (rhsLength*2) stack
          let topState = currentState stack1
          let toState =
               case lookupGotoTable gotoTbl topState lhs of
                 Just state -> state
                 Nothing -> throw (NotFoundGoto topState lhs stack actionTbl gotoTbl prodRules terminalList maybeStatus)
  
          let stack2 = push (StkNonterminal (Just ast) lhs) stack1
          let stack3 = push (StkState toState) stack2
          run terminalList stack3 maybeStatus

debug :: Bool -> String -> IO ()
debug flag msg = if flag then putStrLn msg else return ()

prlevel :: Int -> String
prlevel n = take n (let spaces = ' ' : spaces in spaces)

-- | Computing candidates

data Candidate =     -- Todo: data Candidate vs. data EmacsDataItem = ... | Candidate String 
    TerminalSymbol String
  | NonterminalSymbol String
  deriving Eq

instance Show Candidate where
  showsPrec p (TerminalSymbol s) = (++) $ "Terminal " ++ s
  showsPrec p (NonterminalSymbol s) = (++) $ "Nonterminal " ++ s

data Automaton token ast =
  Automaton {
    actTbl    :: ActionTable,
    gotoTbl   :: GotoTable,
    prodRules :: ProdRules
  }

data CompCandidates token ast = CompCandidates {
    cc_debugFlag :: Bool,
    cc_printLevel :: Int,  
    cc_maxLevel :: Int,  
    
    cc_r_level :: Int,         -- for new algorithm
    cc_gs_level :: Int,        --
    
    cc_simpleOrNested :: Bool,
    cc_automaton :: Automaton token ast,
    cc_searchState :: SearchState
  }

compCandidates
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast
     -> Int
     -> [Candidate]
     -> Int
     -> Stack token ast
     -> IO [[Candidate]]

compCandidates ccOption level symbols state stk = do
  let flag = cc_debugFlag ccOption
  debug flag $ ""
  debug flag $ "[compCandidates] "
  debug flag $ " - state: " ++ show state
  debug flag $ " - stack: " ++ prStack stk
  debug flag $ ""
  -- compGammasDfs ccOption level symbols state stk []
  extendedCompCandidates ccOption symbols state stk
  
compGammasDfs
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast
     -> Int
     -> [Candidate]
     -> Int
     -> Stack token ast
     -> [(Int, Stack token ast, String)]
     -> IO [[Candidate]]

compGammasDfs ccOption level symbols state stk history =
  let flag = cc_debugFlag ccOption
      maxLevel = cc_maxLevel ccOption
      printLevel = cc_printLevel ccOption
      isSimple = cc_simpleOrNested ccOption
      automaton = cc_automaton ccOption
      
      actionTable = actTbl automaton
      gotoTable = gotoTbl automaton
      productionRules = prodRules automaton
  in
  if level > maxLevel then do
    let result_symbols = if null symbols then [] else [symbols]
    debug flag $ prlevel level ++ "maxlevel reached."
    debug flag $ prlevel level ++ " - " ++ show result_symbols
    return result_symbols
  else
    checkCycle flag False level state stk "" history
     (\history ->
       {- 1. Reduce -}
       case nub [prnum | ((s,lookahead),Reduce prnum) <- actionTable, state==s, isReducible productionRules prnum stk] of
         [] -> do
           {- 2. Goto table -}
           debug flag $ prlevel level ++ "no reduce: " ++ show state
           case nub [(nonterminal,toState) | ((fromState,nonterminal),toState) <- gotoTable, state==fromState] of
             [] -> do
               debug flag $ prlevel level ++ "no goto: " ++ show state
               {- 3. Accept -}
               if length [True | ((s,lookahead),Accept) <- actionTable, state==s] >= 1
               then do
                      debug flag $ prlevel level ++ "accept: " ++ show state
                      return []
               {- 4. Shift -}
               else let cand2 = nub [(terminal,snext) | ((s,terminal),Shift snext) <- actionTable, state==s] in
                    let len = length cand2 in
                    case cand2 of
                     [] -> do
                       debug flag $ prlevel level ++ "no shift: " ++ show state
                       return []

                     _  -> do listOfList <-
                                mapM (\ ((terminal,snext),i)-> do
                                   let stk1 = push (StkTerminal (Terminal terminal 0 0 Nothing)) stk  -- Todo: ??? (toToken terminal)
                                   let stk2 = push (StkState snext) stk1

                                   debug flag $ prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "]: "
                                             ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext
                                   debug flag $ prlevel level ++ " - " ++ "Stack " ++ prStack stk2
                                   debug flag $ prlevel level ++ " - " ++ "Symbols: " ++ show (symbols++[TerminalSymbol terminal])
                                   debug flag $ ""

                                   checkCycle flag True level snext stk2 terminal history
                                     (\history1 -> 
                                      compGammasDfs ccOption (level+1) (symbols++[TerminalSymbol terminal]) snext stk2 history1) )
                                        (zip cand2 [1..])
                              return $ concat listOfList
             nontermStateList -> do
               let len = length nontermStateList

               listOfList <-
                 mapM (\ ((nonterminal,snext),i) ->
                    let stk1 = push (StkNonterminal Nothing nonterminal) stk
                        stk2 = push (StkState snext) stk1
                    in 
                    -- checkCycle False level snext stk2 ("GOTO " ++ show snext ++ " " ++ nonterminal) history
                    -- checkCycle True level state stk nonterminal history
                    checkCycle flag True level snext stk2 nonterminal history

                      (\history1 -> do
                       debug flag $ prlevel level ++ "GOTO [" ++ show i ++ "/" ++ show len ++ "] at "
                                ++ show state ++ " -> " ++ show nonterminal ++ " -> " ++ show snext
                       debug flag $ prlevel level ++ " - " ++ "Stack " ++ prStack stk2
                       debug flag $ prlevel level ++ " - " ++ "Symbols:" ++ show (symbols++[NonterminalSymbol nonterminal])
                       debug flag $ ""

                       compGammasDfs ccOption (level+1) (symbols++[NonterminalSymbol nonterminal]) snext stk2 history1) )
                         (zip nontermStateList [1..])
               return $ concat listOfList

         prnumList -> do
           let len = length prnumList

           debug flag $ prlevel level     ++ "# of prNumList to reduce: " ++ show len ++ " at State " ++ show state
           debug flag $ prlevel (level+1) ++ show [ productionRules !! prnum | prnum <- prnumList ]

           do listOfList <-
               mapM (\ (prnum,i) -> (
                 -- checkCycle False level state stk ("REDUCE " ++ show prnum) history
                 checkCycle flag True level state stk (show prnum) history
                   (\history1 -> do
                      debug flag $ prlevel level ++ "REDUCE"
                                      ++ " [" ++ show i ++ "/" ++ show len ++ "]"
                      debug flag $ prlevel level ++ " - prod rule: " ++ show (productionRules !! prnum)
                      debug flag $ prlevel level ++ " - State " ++ show state  
                      debug flag $ prlevel level ++ " - Stack " ++ prStack stk
                      debug flag $ prlevel level ++ " - Symbols: " ++ show symbols
                      debug flag $ ""
                      compGammasDfsForReduce ccOption level symbols state stk history1 prnum)) )
                    (zip prnumList [1..])
              return $ concat listOfList )
  
compGammasDfsForReduce ccOption level symbols state stk history prnum = 
  let flag = cc_debugFlag ccOption
      isSimple = cc_simpleOrNested ccOption
      automaton = cc_automaton ccOption

      prodrule   = (prodRules automaton) !! prnum
      lhs = fst prodrule
      rhs = snd prodrule
      
      rhsLength = length rhs
  in 
  if ( {- rhsLength == 0 || -} (rhsLength > length symbols) ) == False
  then do
    debug flag $ prlevel level ++ "[LEN COND: False] length rhs > length symbols: NOT "
                   ++ show rhsLength ++ ">" ++ show (length symbols)
    debug flag $ "rhs: " ++ show rhs
    debug flag $ "symbols: " ++ show symbols
    return [] -- Todo: (if null symbols then [] else [symbols])
    
  else do   -- reducible
    let stk1 = drop (rhsLength*2) stk
    let topState = currentState stk1
    let toState =
         case lookupGotoTable (gotoTbl automaton) topState lhs of
           Just state -> state
           Nothing -> error $ "[compGammasDfsForReduce] Must not happen: lhs: "
                                ++ lhs ++ " state: " ++ show topState
    let stk2 = push (StkNonterminal Nothing lhs) stk1  -- ast
    let stk3 = push (StkState toState) stk2
    debug flag $ prlevel level ++ " - GOTO : "
                   ++ show topState ++ " " ++ lhs ++ " " ++ show toState
    debug flag $ prlevel level ++ " --- Stack " ++ prStack stk3
    debug flag $ ""

    debug flag $ prlevel level ++ "Found a gamma: " ++ show symbols
    debug flag $ ""

    if isSimple  && not (null symbols) -- Todo: loosley simple mode:  +  && not (null symbols)
    then return (if null symbols then [] else [symbols])
    else do listOfList <- compGammasDfs ccOption (level+1) [] toState stk3 history
            return (if null symbols then listOfList else (symbols : map (symbols ++) listOfList))

--------------------------------------------------------------------------------
-- A new search algorithm
--------------------------------------------------------------------------------
type R_Level  = Int
type GS_Level = Int

data SearchState =
    SS_InitReduces R_Level GS_Level -- Reduce^*
  | SS_GotoOrShift R_Level GS_Level -- (Goto | Shift)
  | SS_FinalReduce R_Level GS_Level -- Reduce
  deriving Show

init_r_level :: R_Level
init_r_level = 1

init_gs_level :: GS_Level
init_gs_level = 5

initSearchState r gs = SS_InitReduces r gs

isInitReduces (SS_InitReduces _ _) = True
isInitReduces _                    = False

isFinalReduce (SS_FinalReduce _ _) = True
isFinalReduce _                    = False

r_level (SS_InitReduces r gs) = r
r_level (SS_GotoOrShift r gs) = r
r_level (SS_FinalReduce r gs) = r

gs_level (SS_InitReduces r gs) = gs
gs_level (SS_GotoOrShift r gs) = gs
gs_level (SS_FinalReduce r gs) = gs

--
extendedCompCandidates
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [Candidate] -> Int -> Stack token ast -> IO [[Candidate]]
extendedCompCandidates ccOption symbols state stk = do
  maybeConfig <- readConfig
  case maybeConfig of
    Nothing ->
      do extendedCompCandidates' ccOption symbols state stk
         
    Just config ->
      let r_level  = config_R_LEVEL config
          gs_level = config_GS_LEVEL config
          debugFlag = config_DEBUG config
          display  = config_DISPLAY config
          isSimple = config_SIMPLE config

          ccOption' = ccOption { cc_debugFlag = debugFlag
                               , cc_r_level = r_level
                               , cc_gs_level = gs_level
                               , cc_simpleOrNested = isSimple
                               , cc_searchState = initSearchState r_level gs_level
                               }
                      
      in extendedCompCandidates' ccOption' symbols state stk

  where
    extendedCompCandidates' ccOption symbols state stk =
      let
         debugFlag = cc_debugFlag ccOption
         isSimple  = cc_simpleOrNested ccOption
         r_level   = cc_r_level ccOption
         gs_level  = cc_gs_level ccOption
      in
      do debug debugFlag $ "simple(True)/nested(False): " ++ show isSimple
         debug debugFlag $ "(Max) r level: " ++ show r_level
         debug debugFlag $ "(Max) gs level: " ++ show gs_level
         debug debugFlag $ ""

         list <- if isSimple
                   then repReduce ccOption symbols state stk 
                   else do extendedNestedCandidates ccOption [(state, stk, symbols)]
                   
         return [ c | (state, stk, c) <- list, null c == False ]


-- Extended simple candidates
extendedSimpleCandidates
    :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
       CompCandidates token ast -> Int -> Stack token ast -> IO [(Int, Stack token ast, [Candidate])]
       
extendedSimpleCandidates ccOption state stk = repReduce ccOption [] state stk 


-- Extended nested candidates
extendedNestedCandidates
    :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
       CompCandidates token ast -> [(Int, Stack token ast, [Candidate])]
       -> IO [(Int, Stack token ast, [Candidate])]
       
extendedNestedCandidates ccOption initStateStkCandsList =
  let f (state, stk, symbols) =
        do debug debugFlag $ "Given "
           debug debugFlag $ " - state " ++ show state
           debug debugFlag $ " - stack " ++ prStack stk
           debug debugFlag $ " - cand  " ++ show symbols
           debug debugFlag $ ""
           
           repReduce ccOption{cc_simpleOrNested=True} symbols state stk

      debugFlag = cc_debugFlag ccOption
      r_level   = cc_r_level ccOption
  in
  if r_level > 0
  then
    do debug debugFlag $ "[extendedNestedCandidates] :"
       mapM_ (\(state, stk, cand) ->
                 do debug debugFlag $ " - state " ++ show state
                    debug debugFlag $ " - stack " ++ prStack stk
                    debug debugFlag $ " - cand  " ++ show cand
                    debug debugFlag $ ""
             ) initStateStkCandsList

       stateStkCandsListList <- mapM f initStateStkCandsList

       if null stateStkCandsListList
         then return initStateStkCandsList
         else do nextStateStkCandsList <-
                   extendedNestedCandidates ccOption{cc_r_level=r_level-1}
                      [ (toState, toStk, fromCand ++ toCand)

                      | ((fromState, fromStk, fromCand), toList)
                          <- zip initStateStkCandsList stateStkCandsListList

                      , (toState, toStk, toCand) <- toList
                      ]

                 return $ initStateStkCandsList ++ nextStateStkCandsList

  else
    return []

repReduce
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [Candidate] -> Int -> Stack token ast
     -> IO [(Int, Stack token ast, [Candidate])]

repReduce ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      searchState     = cc_searchState ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in -- do debug flag $ prlevel level ++ "[repReduce] " ++ show (cc_searchState ccOption)

     if null [True | ((s,lookahead),Accept) <- actionTable, state==s] == False
     then do
            debug flag $ prlevel level ++ "accept: " ++ show state
            return []

     else do
            case nub [prnum | ((s,lookahead),Reduce prnum) <- actionTable
                             , state==s
                             , isReducible productionRules prnum stk] of
               []        -> if isFinalReduce (cc_searchState ccOption)
                            then return []

                            else repGotoOrShift
                                   (ccOption {cc_searchState =
                                              SS_GotoOrShift
                                                (r_level (cc_searchState ccOption))
                                                (gs_level (cc_searchState ccOption)) })
                                     symbols state stk

               prnumList -> do let len = length prnumList

                               listOfList <-
                                 mapM (\ (prnum, i) ->
                                         do let searchState = cc_searchState ccOption
                                            
                                            -- SS_InitReduces
                                            if isInitReduces searchState then
                                              do list2 <- repGotoOrShift
                                                           (ccOption {cc_searchState =
                                                                        SS_GotoOrShift
                                                                          (r_level (cc_searchState ccOption))
                                                                          (gs_level (cc_searchState ccOption)) })
                                                             symbols state stk

                                                 list1 <- simulReduce ccOption symbols prnum len i state stk
                                                 return $ list2 ++ list1

                                            -- SS_FinalReduce
                                            else if isFinalReduce searchState then
                                              do simulReduce ccOption symbols prnum len i state stk

                                            -- SS_GotoOrShift: never reach here!
                                            else
                                              do error $ "repReduce: Unexpected search state: " ++ show searchState)

                                   (zip prnumList [1..])

                               -- list2 <- if isFinalReduce (cc_searchState ccOption)
                               --          then do return []

                               --          else repGotoOrShift
                               --                 (ccOption {cc_searchState =
                               --                              SS_GotoOrShift
                               --                                (r_level (cc_searchState ccOption))
                               --                                (gs_level (cc_searchState ccOption)) })
                               --                   symbols state stk

                               return $ concat listOfList

simulReduce ccOption symbols prnum len i state stk =
  let flag      = cc_debugFlag ccOption
      isSimple  = cc_simpleOrNested ccOption
      automaton = cc_automaton ccOption
      searchState     = cc_searchState ccOption
      level = cc_printLevel ccOption

      productionRules = prodRules automaton
      prodrule  = (prodRules automaton) !! prnum
      lhs       = fst prodrule
      rhs       = snd prodrule
      
      rhsLength = length rhs
  in
    do
       -- debug flag $ prlevel level ++ "[simulReduce] " ++ show (cc_searchState ccOption)

       debug flag $ prlevel level ++ "REDUCE [" ++ show i ++ "/" ++ show len ++ "] at "
       debug flag $ prlevel level ++ " - prod rule: " ++ show (productionRules !! prnum)
       debug flag $ prlevel level ++ " - State " ++ show state  
       debug flag $ prlevel level ++ " - Stack " ++ prStack stk
       debug flag $ prlevel level ++ " - Symbols: " ++ show symbols
       debug flag $ prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)
       debug flag $ ""

       if (rhsLength > length symbols) == False && False -- Q: 필요? False for the moment!
       then do return []

       else do let stk1 = drop (rhsLength*2) stk
               let topState = currentState stk1
               let toState = case lookupGotoTable (gotoTbl automaton) topState lhs of
                     Just state -> state
                     Nothing -> error $ "[simulReduce] Must not happen: lhs: "
                                        ++ lhs ++ " state: " ++ show topState
               let stk2 = push (StkNonterminal Nothing lhs) stk1  -- ast
               let stk3 = push (StkState toState) stk2

               if isSimple then  -- simple mode

                 if isInitReduces searchState then -- reduces until symbols are found
                   do listOfList <- repReduce ccOption{cc_printLevel=level+1} [] toState stk3

                      let f syms0 (s, stk, syms) = (s, stk, syms0 ++ syms)

                      return (if null symbols
                              then listOfList
                              else {- (toState, stk3, symbols) : -} map (f symbols) listOfList)  -- Q: symbols: 필요?
                 else if isFinalReduce searchState then
                   do return (if null symbols
                           then []
                           else [(toState, stk3, symbols)])
                 else -- SS_GotoOrShift
                   do error $ "simulReduce: Unexpected search state" ++ show searchState
                    
               else -- nested mode
                 do error $ "simulReduce: Unexpected nested mode: "


simulGoto ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in do -- debug flag $ prlevel level ++ "[simulGoto] " ++ show (cc_searchState ccOption)

        case nub [ (nonterminal,toState)
                 | ((fromState,nonterminal),toState) <- gotoTable
                 , state==fromState ] of
          [] -> do return []

          nontermStateList ->
            do
              let len = length nontermStateList
              listOfList <-
                mapM (\ ((nonterminal,snext),i) -> do
                          let stk1 = push (StkNonterminal Nothing nonterminal) stk
                          let stk2 = push (StkState snext) stk1

                          debug flag $ prlevel level ++ "GOTO [" ++ show i ++ "/" ++ show len ++ "] at "
                                         ++ show state ++ " -> " ++ show nonterminal ++ " -> " ++ show snext
                          debug flag $ prlevel level ++ " - " ++ "Stack " ++ prStack stk2
                          debug flag $ prlevel level ++ " - " ++ "Symbols:" ++ show (symbols++[NonterminalSymbol nonterminal])
                          debug flag $ prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)
                          debug flag $ ""

                          repGotoOrShift 
                            ccOption{cc_printLevel=level+1}
                              (symbols++[NonterminalSymbol nonterminal])
                                snext stk2)
                  (zip nontermStateList [1..])

              return $ concat listOfList

simulShift ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in
  let cand2 = nub [(terminal,snext) | ((s,terminal),Shift snext) <- actionTable, state==s]
      len = length cand2
  in do -- debug flag $ prlevel level ++ "[simulShift] " ++ show (cc_searchState ccOption)

        case cand2 of
         [] -> do return []

         _  -> do
                  listOfList <-
                    mapM (\ ((terminal,snext),i)-> do
                              let stk1 = push (StkTerminal (Terminal terminal 0 0 Nothing)) stk
                              let stk2 = push (StkState snext) stk1

                              debug flag $ prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "]: "
                                        ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext
                              debug flag $ prlevel level ++ " - " ++ "Stack " ++ prStack stk2
                              debug flag $ prlevel level ++ " - " ++ "Symbols: " ++ show (symbols++[TerminalSymbol terminal])
                              debug flag $ prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)
                              debug flag $ ""

                              repGotoOrShift
                                ccOption{cc_printLevel=level+1}
                                  (symbols++[TerminalSymbol terminal])
                                    snext stk2)
                      (zip cand2 [1..])

                  return $ concat listOfList

repGotoOrShift
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [Candidate] -> Int -> Stack token ast -> IO [(Int, Stack token ast, [Candidate])]

repGotoOrShift ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in do -- debug flag $ prlevel level ++ "[repGotoOrShift] " ++ show (cc_searchState ccOption)
    
        if null [True | ((s,lookahead),Accept) <- actionTable, state==s] == False
        then do
               debug flag $ prlevel level ++ "accept: " ++ show state
               return []

        else do
                listOfList1 <- repReduce
                                 (ccOption{cc_searchState=
                                             SS_FinalReduce
                                               (r_level (cc_searchState ccOption))
                                               (gs_level (cc_searchState ccOption))})
                                   symbols state stk

                if null listOfList1 -- || isInitReduces (cc_searchState ccOption)
                  then
                       if gs_level (cc_searchState ccOption) - 1 > 0 then
                         let ccOption' = ccOption{cc_searchState=
                                             SS_GotoOrShift
                                               (r_level (cc_searchState ccOption))
                                               (gs_level (cc_searchState ccOption) - 1)}
                         in

                         do listOfList2 <- simulGoto ccOption' symbols state stk
                            listOfList3 <- simulShift ccOption' symbols state stk
                            return $ listOfList1 ++ listOfList2 ++ listOfList3

                       else
                         return listOfList1  -- Q: symbols or []

                  else do return listOfList1

-- Todo: repReduce를 하지 않고
--       Reduce 액션이 있는지 보고
--       없으면 goto or shift 진행하고
--       있으면 reduce한번하고 종료!

--       현재 구현은 repReduce 결과가 널인지 검사해서 진행 또는 종료
--       Reduce 액션이 있어도 진행될 수 있음!

--
isReducible productionRules prnum stk =
  let 
      prodrule   = productionRules !! prnum
      rhs = snd prodrule
      
      rhsLength = length rhs
      
      prefix_stk = take (rhsLength*2) stk
      reducible =
        isMatched rhs
          (reverse [elem | (i,elem) <- zip [1..] prefix_stk, i `mod` 2 == 0])
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

-- | Cycle checking
noCycleCheck :: Bool
noCycleCheck = True

checkCycle debugflag flag level state stk action history cont =
  if flag && (state,stk,action) `elem` history
  then do
    debug debugflag $ prlevel level ++ "CYCLE is detected !!"
    debug debugflag $ prlevel level ++ " - " ++ show state ++ " " ++ action
    debug debugflag $ prlevel level ++ " - " ++ prStack stk
    debug debugflag $ ""
    return []
  else cont ( (state,stk,action) : history )

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
    nonterminalToStringMaybe :: Maybe (String->String)
  }

-- | handleParseError
-- handleParseError :: TokenInterface token => Bool -> Int -> Bool -> [Terminal token] -> ParseError token ast -> IO [EmacsDataItem]
-- handleParseError flag maxLevel isSimple terminalListAfterCursor parseError =
--   unwrapParseError flag maxLevel isSimple terminalListAfterCursor parseError
  
handleParseError :: TokenInterface token => HandleParseError token -> ParseError token ast -> IO [EmacsDataItem]
handleParseError hpeOption parseError =
  do maybeConfig <- readConfig
  
     flag <- case maybeConfig of
               Nothing -> return $ debugFlag hpeOption
               Just config -> return $ config_DEBUG config

     unwrapParseError hpeOption{debugFlag = flag} parseError

  --
unwrapParseError hpeOption (NotFoundAction _ state stk _actTbl _gotoTbl _prodRules terminalList maybeStatus) = do
    let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules}
    arrivedAtTheEndOfSymbol hpeOption state stk automaton terminalList maybeStatus
    
unwrapParseError hpeOption (NotFoundGoto state _ stk _actTbl _gotoTbl _prodRules terminalList maybeStatus) = do
    let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules}
    arrivedAtTheEndOfSymbol hpeOption state stk automaton terminalList maybeStatus

--
arrivedAtTheEndOfSymbol hpeOption state stk automaton [_] maybeStatus =  -- [$]
  case maybeStatus of
    Nothing -> do
      debug (debugFlag hpeOption) $ "No saved stack"
      _handleParseError hpeOption state stk automaton
    Just savedStk ->
      let savedStk = fromJust maybeStatus
          savedState = currentState savedStk
      in do
          debug (debugFlag hpeOption) $ "Restored stack"
          debug (debugFlag hpeOption) $ " - state: " ++ show savedState
          debug (debugFlag hpeOption) $ " - stack: " ++ prStack savedStk
          _handleParseError hpeOption savedState savedStk automaton
              
arrivedAtTheEndOfSymbol hpeOption state stk automaton terminalList maybeStatus = do
     -- debug (debugFlag hpeOption) $ "length terminalList /= 1 : " ++ show (length terminalList)
     -- debug (debugFlag hpeOption) $ map (\t -> terminalToString $ t) terminalList
     return [SynCompInterface.ParseError (map terminalToString terminalList)]

_handleParseError
  (hpeOption @ HandleParseError {
      debugFlag=flag,
      searchMaxLevel=maxLevel,
      simpleOrNested=isSimple,
      postTerminalList=terminalListAfterCursor,
      nonterminalToStringMaybe=_nonterminalToStringMaybe})
  state stk automaton = do
  let ccOption = CompCandidates {
        cc_debugFlag=flag,
        cc_printLevel=0,
        cc_maxLevel=maxLevel,
        cc_simpleOrNested=isSimple,
        cc_automaton=automaton,
        cc_searchState = initSearchState init_r_level init_gs_level,
        cc_r_level = init_r_level,  
        cc_gs_level = init_gs_level} 
  candidateListList <- compCandidates ccOption 0 [] state stk
  let colorListList_symbols =
       [ filterCandidates candidateList terminalListAfterCursor
       | candidateList <- candidateListList ]
  let convFun =
        case _nonterminalToStringMaybe of
          Nothing -> \s -> "..."
          Just fn -> fn
  let colorListList_ = map (stringfyCandidates convFun) colorListList_symbols
  let colorListList = map collapseCandidates colorListList_
  let strList = nub [ concatStrList strList | strList <- map (map showEmacsColor) colorListList ]
  let rawStrListList = nub [ strList | strList <- map (map showRawEmacsColor) colorListList ]

  debug (flag || True) $ ""
  mapM_ (debug (flag || True)) $ map show colorListList_symbols
  
  debug (flag || True) $ ""
  mapM_ (debug (flag || True)) $ map show rawStrListList
  
  debug (flag || True) $ ""
  
  -- debug (flag || True) $ showConcat $ map (\x -> (show x ++ "\n")) colorListList_symbols
  -- debug (flag || True) $ showConcat $ map (\x -> (show x ++ "\n")) rawStrListList -- mapM_ (putStrLn . show) rawStrListList
  
  return $ map Candidate strList
  
  where
    showConcat [] = ""
    showConcat (s:ss) = s ++ " " ++ showConcat ss
    
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
