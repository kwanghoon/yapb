{-# LANGUAGE GADTs #-}
module CommonParserUtil where

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

import SynCompInterface 

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

prLexError (CommonParserUtil.LexError line col text) = do
  putStr $ "No matching lexer spec at "
  putStr $ "Line " ++ show line
  putStr $ "Column " ++ show col
  putStr $ " : "
  putStr $ take 10 text

--
lexing :: TokenInterface token =>
          LexerSpec token -> String -> IO [Terminal token]
lexing lexerspec text = lexing_ lexerspec 1 1 text

lexing_ :: TokenInterface token =>
           LexerSpec token -> Line -> Column -> String -> IO [Terminal token]
lexing_ lexerspec line col [] = do
  let eot = endOfToken lexerspec 
  return [Terminal (fromToken eot) line col eot]
   
lexing_ lexerspec line col text = do
  (matchedText, theRestText, maybeTok) <-
    matchLexSpec line col (lexerSpecList lexerspec) text
  let (line_, col_) = moveLineCol line col matchedText
  terminalList <- lexing_ lexerspec line_ col_ theRestText
  case maybeTok of
    Nothing  -> return terminalList
    Just tok -> do
      let terminal = Terminal matchedText line col tok
      return (terminal:terminalList)

matchLexSpec :: TokenInterface token =>
                Line -> Column -> LexerSpecList token -> String
             -> IO (String, String, Maybe token)
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
    "" -> return (matched, post, tokenBuilder matched)
    _  -> matchLexSpec line col lexerspec text


moveLineCol :: Line -> Column -> String -> (Line, Column)
moveLineCol line col ""          = (line, col)
moveLineCol line col ('\n':text) = moveLineCol (line+1) 1 text
moveLineCol line col (ch:text)   = moveLineCol line (col+1) text
  
--------------------------------------------------------------------------------  
-- The parsing machine
--------------------------------------------------------------------------------

--
data ParseError token ast where
    -- teminal, state, stack actiontbl, gototbl
    NotFoundAction :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
      (Terminal token) -> Int -> (Stack token ast) -> ActionTable -> GotoTable -> ProdRules -> [Terminal token] -> ParseError token ast
    
    -- topState, lhs, stack, actiontbl, gototbl,
    NotFoundGoto :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
       Int -> String -> (Stack token ast) -> ActionTable -> GotoTable -> ProdRules -> [Terminal token] -> ParseError token ast

  deriving (Typeable)

instance (Show token, Show ast) => Show (ParseError token ast) where
  showsPrec p (NotFoundAction terminal state stack _ _ _ _) =
    (++) "NotFoundAction" . (++) (terminalToString terminal) . (++) (show state) -- . (++) (show stack)
  showsPrec p (NotFoundGoto topstate lhs stack _ _ _ _) =
    (++) "NotFoundGoto" . (++) (show topstate) . (++) lhs -- . (++) (show stack)

instance (TokenInterface token, Typeable token, Show token, Typeable ast, Show ast)
  => Exception (ParseError token ast)

prParseError (NotFoundAction terminal state stack actiontbl gototbl prodRules terminalList) = do
  putStrLn $
    ("Not found in the action table: "
     ++ terminalToString terminal)
     ++ " : "
     ++ show (state, tokenTextFromTerminal terminal)
     ++ " (" ++ show (length terminalList) ++ ")"
     ++ "\n" ++ prStack stack ++ "\n"
     
prParseError (NotFoundGoto topState lhs stack actiontbl gototbl prodRules terminalList) = do
  putStrLn $
    ("Not found in the goto table: ")
     ++ " : "
     ++ show (topState,lhs) ++ "\n"
     ++ " (" ++ show (length terminalList) ++ ")"
     ++ prStack stack ++ "\n"

--
parsing :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
           ParserSpec token ast -> [Terminal token] -> IO ast
parsing parserSpec terminalList = do
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
  ast <- runAutomaton actionTbl gotoTbl prodRules pFunList terminalList
  
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
prStack (StkTerminal (Terminal text _ _ token) : stack) =
  let str_token = fromToken token in
  (if str_token == text then str_token else (fromToken token ++ " i.e. " ++ text))
    ++  " : " ++ prStack stack
prStack (StkNonterminal _ str : stack) = str ++ " : " ++ prStack stack

-- Utility for Automation
currentState :: Stack token ast -> Int
currentState (StkState i : stack) = i
currentState _                    = error "No state found in the stack top"

tokenTextFromTerminal :: TokenInterface token => Terminal token -> String
tokenTextFromTerminal (Terminal _ _ _ token) = fromToken token

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

initState = 0

type ParseFunList token ast = [ParseFun token ast]

runAutomaton :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
  {- static part -}
  ActionTable -> GotoTable -> ProdRules -> ParseFunList token ast -> 
  {- dynamic part -}
  [Terminal token] ->
  {- AST -}
  IO ast
runAutomaton actionTbl gotoTbl prodRules pFunList terminalList = do
  let initStack = push (StkState initState) emptyStack
  run terminalList initStack
  
  where
    {- run :: TokenInterface token => [Terminal token] -> Stack token ast -> IO ast -}
    run terminalList stack = do
      let state = currentState stack
      let terminal = head terminalList
      let text  = tokenTextFromTerminal terminal
      let action =
           case lookupActionTable actionTbl state terminal of
             Just action -> action
             Nothing -> throw (NotFoundAction terminal state stack actionTbl gotoTbl prodRules terminalList)
                        -- error $ ("Not found in the action table: "
                        --          ++ terminalToString terminal)
                        --          ++ " : "
                        --          ++ show (state, tokenTextFromTerminal terminal)
                        --          ++ "\n" ++ prStack stack ++ "\n"
      
      debug ("\nState " ++ show state)
      debug ("Token " ++ text)
      debug ("Stack " ++ prStack stack)
      
      case action of
        Accept -> do
          debug "Accept"
          
          case stack !! 1 of
            StkNonterminal (Just ast) _ -> return ast
            StkNonterminal Nothing _ -> fail "Empty ast in the stack nonterminal"
            _ -> fail "Not Stknontermianl on Accept"
        
        Shift toState -> do
          debug ("Shift " ++ show toState)
          
          let stack1 = push (StkTerminal (head terminalList)) stack
          let stack2 = push (StkState toState) stack1
          run (tail terminalList) stack2
          
        Reduce n -> do
          debug ("Reduce " ++ show n)
          
          let prodrule   = prodRules !! n
          
          debug ("\t" ++ show prodrule)
          
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
                 Nothing -> throw (NotFoundGoto topState lhs stack actionTbl gotoTbl prodRules terminalList)
                            -- error $ ("Not found in the goto table: ")
                            --         ++ " : "
                            --         ++ show (topState,lhs) ++ "\n"
                            --         ++ prStack stack ++ "\n"
  
          let stack2 = push (StkNonterminal (Just ast) lhs) stack1
          let stack3 = push (StkState toState) stack2
          run terminalList stack3

flag = True

debug :: String -> IO ()
debug msg = if flag then putStrLn msg else return ()

prlevel n = take n (let spaces = ' ' : spaces in spaces)

--
data Candidate =
    TerminalSymbol String
  | NonterminalSymbol String
  deriving (Show,Eq)

data Automaton token ast =
  Automaton {
    actTbl    :: ActionTable,
    gotoTbl   :: GotoTable,
    prodRules :: ProdRules
  }

compCandidates isSimple level symbols state automaton stk = do
  compGammas isSimple level symbols state automaton stk []
--  gammas <- compGammas isSimple level symbols state automaton stk []
--  if isSimple
--  then return gammas
--  else return $ tail $ scanl (++) [] (filter (not . null) gammas)

compGammas :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
  Bool -> Int -> [Candidate] -> Int -> Automaton token ast -> Stack token ast -> [(Int, Stack token ast, String)]-> IO [[Candidate]]

checkCycle flag level state stk action history cont =
  if flag && (state,stk,action) `elem` history
  then do debug $ prlevel level ++ "CYCLE is detected !!"
          debug $ prlevel level ++ show state ++ " " ++ action
          debug $ prlevel level ++ prStack stk
          debug $ ""
          return []
  else cont ( (state,stk,action) : history )

compGammas isSimple level symbols state automaton stk history = 
  checkCycle False level state stk "" history
   (\history -> 
     case nub [prnum | ((s,lookahead),Reduce prnum) <- actTbl automaton, state==s] of
      [] ->
        case nub [(nonterminal,toState) | ((fromState,nonterminal),toState) <- gotoTbl automaton, state==fromState] of
          [] ->
            if length [True | ((s,lookahead),Accept) <- actTbl automaton, state==s] >= 1
            then do 
                   return []
            else let cand2 = nub [(terminal,snext) | ((s,terminal),Shift snext) <- actTbl automaton, state==s] in
                 let len = length cand2 in
                 case cand2 of
                  [] -> return []
               
                  _  -> do listOfList <-
                             mapM (\ ((terminal,snext),i)->
                                let stk1 = push (StkTerminal (Terminal terminal 0 0 (toToken terminal))) stk
                                    stk2 = push (StkState snext) stk1
                                in 
                                -- checkCycle False level snext stk2 ("SHIFT " ++ show snext ++ " " ++ terminal) history
                                -- checkCycle True level state stk terminal history
                                checkCycle True level snext stk2 terminal history
                             
                                  (\history1 -> do
                                   debug $ prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "]: "
                                             ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext
                                   debug $ prlevel level ++ "Goto/Shift symbols: " ++ show (symbols++[TerminalSymbol terminal])
                                   debug $ prlevel level ++ "Stack " ++ prStack stk2
                                   debug $ ""
                                   compGammas isSimple (level+1) (symbols++[TerminalSymbol terminal]) snext automaton stk2 history1) )
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
                 checkCycle True level snext stk2 nonterminal history
              
                   (\history1 -> do
                    debug $ prlevel level ++ "GOTO [" ++ show i ++ "/" ++ show len ++ "] at "
                             ++ show state ++ " -> " ++ show nonterminal ++ " -> " ++ show snext
                    debug $ prlevel level ++ "Goto/Shift symbols:" ++ show (symbols++[NonterminalSymbol nonterminal])
                    debug $ prlevel level ++ "Stack " ++ prStack stk2
                    debug $ ""
      
                    compGammas isSimple (level+1) (symbols++[NonterminalSymbol nonterminal]) snext automaton stk2 history1) )
                      (zip nontermStateList [1..])
            return $ concat listOfList

      prnumList -> do
        let len = length prnumList
     
        debug $ prlevel level     ++ "# of prNumList to reduce: " ++ show len ++ " at State " ++ show state
        debug $ prlevel (level+1) ++ show [ (prodRules automaton) !! prnum | prnum <- prnumList ]
     
        -- let aCandidate = if null symbols then [] else [symbols]
        -- if isSimple
        -- then return aCandidate
        -- else do listOfList <-
        do listOfList <-
            mapM (\ (prnum,i) -> (
              -- checkCycle False level state stk ("REDUCE " ++ show prnum) history
              checkCycle True level state stk (show prnum) history
                (\history1 -> do
                   debug $ prlevel level ++ "State " ++ show state  ++ "[" ++ show i ++ "/" ++ show len ++ "]" 
                   debug $ prlevel level ++ "REDUCE" ++ " prod #" ++ show prnum
                   debug $ prlevel level ++ show ((prodRules automaton) !! prnum)
                   debug $ prlevel level ++ "Goto/Shift symbols: " ++ show symbols
                   debug $ prlevel level ++ "Stack " ++ prStack stk
                   debug $ ""
                   compGammasForReduce level isSimple  symbols state automaton stk history1 prnum)) )
                 (zip prnumList [1..])
           return $ concat listOfList )
  
noCycleCheck :: Bool
noCycleCheck = True

compGammasForReduce level isSimple  symbols state automaton stk history prnum = 
  let prodrule   = (prodRules automaton) !! prnum
      lhs = fst prodrule
      rhs = snd prodrule
      
      rhsLength = length rhs
  in 
  if ( {- rhsLength == 0 || -} (rhsLength > length symbols) ) == False
  then do
    debug $ prlevel level ++ "[LEN COND: False] length rhs > length symbols: NOT " ++ show rhsLength ++ ">" ++ show (length symbols)
    debug $ prlevel (level+1) ++ show symbols
    debug $ prlevel level
    return []
  else do
    let stk1 = drop (rhsLength*2) stk
    let topState = currentState stk1
    let toState =
         case lookupGotoTable (gotoTbl automaton) topState lhs of
           Just state -> state
           Nothing -> error $ "[compGammasForReduce] Must not happen: lhs: " ++ lhs ++ " state: " ++ show topState
    let stk2 = push (StkNonterminal Nothing lhs) stk1  -- ast
    let stk3 = push (StkState toState) stk2
    debug $ prlevel level ++ "GOTO after REDUCE: " ++ show topState ++ " " ++ lhs ++ " " ++ show toState
    debug $ prlevel level ++ "Goto/Shift symbols: " ++ "[]"
    debug $ prlevel level ++ "Stack " ++ prStack stk3
    debug $ ""

    debug $ prlevel level ++ "Found a gamma: " ++ show symbols
    debug $ ""

    if isSimple
    then return (if null symbols then [] else [symbols])
    else do listOfList <- compGammas isSimple (level+1) [] toState automaton stk3 history
            return (if null symbols then listOfList else (symbols : map (symbols ++) listOfList))

--
successfullyParsed :: IO [EmacsDataItem]
successfullyParsed = return [SynCompInterface.SuccessfullyParsed]

handleLexError :: IO [EmacsDataItem]
handleLexError = return [SynCompInterface.LexError]
  
handleParseError isSimple (NotFoundAction _ state stk actTbl gotoTbl prodRules terminalList) =
  _handleParseError isSimple state stk actTbl gotoTbl prodRules terminalList
handleParseError isSimple (NotFoundGoto state _ stk actTbl gotoTbl prodRules terminalList) =
  _handleParseError isSimple state stk actTbl gotoTbl prodRules terminalList


_handleParseError isSimple state stk _actTbl _gotoTbl _prodRules terminalList = 
   if length terminalList == 1 then do -- [$]
     let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules}
     candidates <- compCandidates isSimple 0 [] state automaton stk
     let cands = candidates
     let strs = nub [ concatStrList strList | strList <- map (map showSymbol) cands ]
     let rawStrs = nub [ strList | strList <- map (map showRawSymbol) cands ]
     mapM_ (putStrLn . show) rawStrs
     return $ map Candidate strs
   else
     return [SynCompInterface.ParseError (map terminalToString terminalList)]

showSymbol (TerminalSymbol s) = s
showSymbol (NonterminalSymbol _) = "..."

showRawSymbol (TerminalSymbol s) = s
showRawSymbol (NonterminalSymbol s) = s

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
