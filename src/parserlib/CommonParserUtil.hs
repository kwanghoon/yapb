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
  runAutomatonHaskell flag amSpec {- initState actionTbl gotoTbl prodRules pFunList-} terminalList Nothing

runAutomatonHaskell :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
  Bool -> 
  {- static part ActionTable -> GotoTable -> ProdRules -> ParseFunList token ast -> -}
  AutomatonSpec token ast -> 
  {- dynamic part -}
  [Terminal token] ->
  {- haskell parser specific option -}
  Maybe token ->
  {- AST -}
  IO ast
runAutomatonHaskell flag (rm_spec @ AutomatonSpec {
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
          putStrLn $ "lookActionTable failed (1st) with: " ++ show (terminalToString terminal)
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
                  putStrLn $ "lookActionTable succeeded (2nd) with: " ++ terminalToString terminal_close_brace
                  runAction state terminal_close_brace action (terminal_close_brace : terminalList) stack maybeStatus
                  
                Nothing -> do
                  putStrLn $ "lookActionTable failed (2nd) with: " ++ terminalToString terminal_close_brace
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
    cc_searchMaxLevel :: Int,
    cc_simpleOrNested :: Bool,
    cc_automaton :: Automaton token ast
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
  compGammasDfs ccOption level symbols state stk []

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
      maxLevel = cc_searchMaxLevel ccOption
      isSimple = cc_simpleOrNested ccOption
      automaton = cc_automaton ccOption
      
      actionTable = actTbl automaton
      gotoTable = gotoTbl automaton
      productionRules = prodRules automaton
  in
  if level > maxLevel then
    return (if null symbols then [] else [symbols])
  else
    checkCycle flag False level state stk "" history
     (\history ->
       {- 1. Reduce -}
       case nub [prnum | ((s,lookahead),Reduce prnum) <- actionTable, state==s, isReducible productionRules prnum stk] of
         [] -> do
           {- 2. Goto table -}
           debug flag $ "no reduce: " ++ show state
           case nub [(nonterminal,toState) | ((fromState,nonterminal),toState) <- gotoTable, state==fromState] of
             [] -> do
               debug flag $ "no goto: " ++ show state
               {- 3. Accept -}
               if length [True | ((s,lookahead),Accept) <- actionTable, state==s] >= 1
               then do 
                      return []
               {- 4. Shift -}
               else let cand2 = nub [(terminal,snext) | ((s,terminal),Shift snext) <- actionTable, state==s] in
                    let len = length cand2 in
                    case cand2 of
                     [] -> do
                       debug flag $ "no shift: " ++ show state
                       return []

                     _  -> do listOfList <-
                                mapM (\ ((terminal,snext),i)->
                                   let stk1 = push (StkTerminal (Terminal terminal 0 0 Nothing)) stk  -- Todo: ??? (toToken terminal)
                                       stk2 = push (StkState snext) stk1
                                   in 
                                   -- checkCycle False level snext stk2 ("SHIFT " ++ show snext ++ " " ++ terminal) history
                                   -- checkCycle True level state stk terminal history
                                   checkCycle flag True level snext stk2 terminal history

                                     (\history1 -> do
                                      debug flag $ prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "]: "
                                                ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext
                                      debug flag $ prlevel level ++ "Goto/Shift symbols: " ++ show (symbols++[TerminalSymbol terminal])
                                      debug flag $ prlevel level ++ "Stack " ++ prStack stk2
                                      debug flag $ ""
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
                       debug flag $ prlevel level ++ "Goto/Shift symbols:" ++ show (symbols++[NonterminalSymbol nonterminal])
                       debug flag $ prlevel level ++ "Stack " ++ prStack stk2
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
    debug flag $ prlevel level ++ "GOTO : "
                   ++ show topState ++ " " ++ lhs ++ " " ++ show toState
    debug flag $ prlevel level ++ "Stack " ++ prStack stk3
    debug flag $ ""

    debug flag $ prlevel level ++ "Found a gamma: " ++ show symbols
    debug flag $ ""

    if isSimple -- Todo: loosley simple mode:  +  && not (null symbols)
    then return (if null symbols then [] else [symbols])
    else do listOfList <- compGammasDfs ccOption (level+1) [] toState stk3 history
            return (if null symbols then listOfList else (symbols : map (symbols ++) listOfList))

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
    debug debugflag $ prlevel level ++ show state ++ " " ++ action
    debug debugflag $ prlevel level ++ prStack stk
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
handleParseError hpeOption parseError = unwrapParseError hpeOption parseError

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
        cc_searchMaxLevel=maxLevel,
        cc_simpleOrNested=isSimple,
        cc_automaton=automaton }
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
  debug flag $ showConcat $ map (\x -> (show x ++ "\n")) colorListList_symbols
  debug flag $ showConcat $ map (\x -> (show x ++ "\n")) rawStrListList -- mapM_ (putStrLn . show) rawStrListList
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
