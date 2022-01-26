{-# LANGUAGE GADTs #-}
module CommonParserUtilWithEndingReduce
  ( LexerSpec(..), ParserSpec(..), AutomatonSpec(..)
  , LexerParserState, Line, Column
  , ProdRuleStr, ParseAction, ProdRulePrec
  , HandleParseError(..), defaultHandleParseError
  , matchLexSpec, LexAction, aLexer
  , lexing, lexingWithLineColumn, _lexingWithLineColumn
  , parsing
  , initState, runAutomaton
  , get, getText
  , LexError(..), ParseError(..), lpStateFrom
  , successfullyParsed, handleLexError, handleParseError)
where

import Attrs
import Terminal
import TokenInterface

import Text.Regex.TDFA
import System.Exit
import System.Process
import Control.Monad
import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class

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
import Debug.Trace (trace)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

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

type Line               = Int
type Column             = Int
type LexerParserState a = (a, Line, Column, String)    -- Lexer and parser states

type LexerParserMonad m a = ST.StateT (LexerParserState a) m

--------------------------------------------------------------------------------
-- | Lexer Specification
--------------------------------------------------------------------------------

type RegExpStr    = String
type LexAction token m a = String -> LexerParserMonad m a (Maybe token)

type RegexprActionList token m a = [(RegExpStr, LexAction token m a)]

data LexerSpec token m a =
  LexerSpec
    { endOfToken    :: token,
      lexerSpecList :: RegexprActionList token m a }

-- | Token precedence and associativity: TokenPrecAssoc token
-- |
-- |    e.g., [ (Nonassoc, [ "integer_number" ])
-- |          , (Left,     [ "+", "-" ])
-- |          , (Left,     [ "*", "/" ])
-- |          , (Right,    [ "UMINUS" ])   -- placeholder
-- |          ]

type TokenPrecAssoc = [(Associativity, [TokenOrPlaceholder])]

--------------------------------------------------------------------------------
-- | Parser Specification
-- |     A -> rhs %prec <token> {action}
--------------------------------------------------------------------------------

type ProdRuleStr = String                       -- A -> rhs
type ParseAction token ast m a =                -- {action}
  Stack token ast -> LexerParserMonad m a ast
type ProdRulePrec = Maybe TokenOrPlaceholder    -- %prec <token>
type ProdRulePrecs = [ProdRulePrec]

type ParserSpecList token ast m a = [(ProdRuleStr, ParseAction token ast m a, ProdRulePrec)]

data ParserSpec token ast m a =
  ParserSpec { startSymbol    :: String,
               tokenPrecAssoc :: TokenPrecAssoc,
               parserSpecList :: ParserSpecList token ast m a,
               baseDir        :: String,   -- ex) ./
               actionTblFile  :: String,   -- ex) actiontable.txt
               gotoTblFile    :: String,   -- ex) gototable.txt
               grammarFile    :: String,   -- ex) grammar.txt
               parserSpecFile :: String,   -- ex) mygrammar.grm
               genparserexe   :: String    -- ex) genlrparse-exe
             }

--------------------------------------------------------------------------------
-- | Stack
--------------------------------------------------------------------------------

data StkElem token ast =
    StkState Int
  | StkTerminal (Terminal token)
  | StkNonterminal (Maybe ast) String -- String for printing Nonterminal instead of ast

instance TokenInterface token => Eq (StkElem token ast) where
  (StkState i)          == (StkState j)          = i == j
  (StkTerminal termi)   == (StkTerminal termj)   =
     tokenTextFromTerminal termi == tokenTextFromTerminal termj
  (StkNonterminal _ si) == (StkNonterminal _ sj) = si == sj
  leftStkElm            == rightStkElm           = False

type Stack token ast = [StkElem token ast]

get :: TokenInterface token => Stack token ast -> Int -> ast
get stack i =
  case stack !! (i-1) of
    StkNonterminal (Just ast) _ -> ast
    StkNonterminal Nothing _ -> error $ "get: empty ast in the nonterminal at stack"
    StkState s -> error $ "get: out of bound: " ++ show i ++ " : state " ++ show s
    StkTerminal terminal -> error $ "get: out of bound: " ++ show i ++ " : terminal " ++ terminalToSymbol terminal

getText :: Stack token ast -> Int -> String
getText stack i = 
  case stack !! (i-1) of
    StkTerminal (Terminal text _ _ _) -> text
    _ -> error $ "getText: out of bound: " ++ show i

emptyStack = []

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

-- -- Specification
-- data Spec token ast =
--   Spec (LexerSpec token) (ParserSpec token ast)

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
          -> (LexerParserState a)
          -> ST.StateT (LexerParserState a) m (MatchedToken token)
     mlsSub debugFlag eot lexerspec (state_parm, line, col, []) =
       do ST.put (state_parm, line+1, 1, [])                  -- EOT at (line+1,1)?
          return (fromToken eot, Just eot, fromToken eot)

     mlsSub debugFlag eot [] (state_parm, line, col, text) = do
       throw (CommonParserUtilWithEndingReduce.LexError line col (takeRet 0 text))

     mlsSub debugFlag eot ((aSpec,tokenBuilder):lexerspec) (state_parm, line, col, text) = do
       let (pre, matched, post) = text =~ aSpec :: (String,String,String)
       case pre of
         "" -> let (line_, col_) = moveLineCol line col matched in
                if line==line_ && col==col_
                then
                  throw (CommonParserUtilWithEndingReduce.LexError line col ("Found RegExp for \"\"? " ++ aSpec))

                else
                  do maybeTok <- tokenBuilder matched

                     let str_maybeTok =
                           if isNothing maybeTok
                           then "Nothing"
                           else (fromToken (fromJust maybeTok))

                     (state_parm_, _, _, _) <- ST.get
                     ST.put (state_parm_, line_, col_, post)

                     debug debugFlag "" $ 

                      debug debugFlag ("Lexer: - " ++ show aSpec ++ " " ++ matched ++ " at " ++ show (line, col)) $
                       debug debugFlag ("       - returns: " ++ if isNothing maybeTok then "Nothing" else str_maybeTok) $

                        return (matched, maybeTok, aSpec)

         _  -> mlsSub debugFlag eot lexerspec (state_parm, line, col, text)

takeRet n [] = ""
takeRet 0 ('\n':text) = '\n' : takeRet 0 text
takeRet n ('\n':text)  = ""
takeRet n (c:text) = c : (takeRet (n+1) text)

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
lexingWithLineColumn lexerspec state_parm line col text =
  _lexingWithLineColumn False lexerspec state_parm line col text


_lexingWithLineColumn :: (Monad m, TokenInterface token) =>
  Bool -> LexerSpec token m a -> a -> Line -> Column -> String -> m [Terminal token]
_lexingWithLineColumn debugFlag lexerspec state_parm line col text =
  do terminalList  <-
       ST.evalStateT
         (repMatchLexSpec debugFlag (endOfToken lexerspec) (lexerSpecList lexerspec))
           (state_parm, line, col, text)

     return terminalList


--------------------------------------------------------------------------------  
-- The parsing machine with parse/lex errors
--------------------------------------------------------------------------------

type CurrentState    = Int
type StateOnStackTop = Int
type LhsSymbol = String

type AutomatonSnapshot token ast =   -- TODO: Refactoring
  (Stack token ast, ActionTable, GotoTable, ProdRules)

--
data ParseError token ast a where
    -- teminal, state, stack actiontbl, gototbl
    NotFoundAction ::
      (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
      (Terminal token) -> CurrentState -> (Stack token ast) ->
      ActionTable -> GotoTable -> ProdRules ->
      LexerParserState a ->  -- [Terminal token]
      Maybe [StkElem token ast] ->
      ParseError token ast a
    
    -- topState, lhs, stack, actiontbl, gototbl,
    NotFoundGoto ::
      (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
      StateOnStackTop -> LhsSymbol -> (Stack token ast) ->
      ActionTable -> GotoTable -> ProdRules ->
      LexerParserState a -> -- [Terminal token]
      Maybe [StkElem token ast] ->
      ParseError token ast a

  deriving (Typeable)

instance (Show token, Show ast) => Show (ParseError token ast a) where
  showsPrec p (NotFoundAction terminal state stack _ _ _ (_,line,col,text) _ ) =
    (++) "NotFoundAction: State " .
    (++) (show state) . (++) " : " .
    (++) (terminalToString terminal) . (++) " " .    -- (++) (show $ length stack)
    (++) "Line " . (++) (show line) . (++) " " .
    (++) "Column " . (++) (show col) . (++) " : " .
    (++) (takeRet 80 text)
    
  showsPrec p (NotFoundGoto topstate lhs stack _ _ _ (_,line,col,text) _) =
    (++) "NotFoundGoto: State " .
    (++) (show topstate) . (++) " ; " .
    (++) lhs . (++) " " .                            -- . (++) (show stack)
    (++) "Line " . (++) (show line) . (++) " " .
    (++) "Column " . (++) (show col) . (++) " : " .
    (++) (takeRet 80 text)

instance (TokenInterface token, Typeable token, Show token, Typeable ast, Show ast, Typeable a)
  => Exception (ParseError token ast a)

lpStateFrom (NotFoundAction _ _ _ _ _ _ lpstate _) = lpstate
lpStateFrom (NotFoundGoto   _ _ _ _ _ _ lpstate _) = lpstate

--------------------------------------------------------------------------------
-- | Automation specification
--------------------------------------------------------------------------------

data AutomatonSpec token ast m a =
  AutomatonSpec {
    am_actionTbl :: ActionTable,
    am_gotoTbl :: GotoTable,
    am_prodRules :: ProdRules,
    am_parseFuns :: ParseActionList token ast m a,
    am_initState :: Int
  }

initState = 0

type ParseActionList token ast m a = [ParseAction token ast m a]


--------------------------------------------------------------------------------
-- | Automaton 
--------------------------------------------------------------------------------

-- Utility for Automation
currentState :: Stack token ast -> Int
currentState (StkState i : stack) = i
currentState _                    = error "No state found in the stack top"

lookupTable :: (Eq a, Show a) => [(a,b)] -> a -> Maybe b
lookupTable tbl key =   
  case [val | (key', val) <- tbl, key==key'] of
    []    -> Nothing
    (h:_) -> Just h

lookupActionTable :: TokenInterface token => ActionTable -> Int -> (Terminal token) -> Maybe Action
lookupActionTable actionTbl state terminal =
  lookupTable actionTbl (state,tokenTextFromTerminal terminal)

lookupGotoTable :: GotoTable -> Int -> String -> Maybe Int
lookupGotoTable gotoTbl state nonterminalStr =
  lookupTable gotoTbl (state,nonterminalStr)

errorKeyword :: String  -- A reserved terminal name : A -> error
errorKeyword = "error"

lookupActionTableWithError :: ActionTable -> Int -> Maybe Action
lookupActionTableWithError actionTbl state =
  case lookupTable actionTbl (state,errorKeyword) of
    Just action          -> Just action   -- This can be either Shift or Reduce!
                            
    Nothing              -> Nothing


-- Note: take 1th, 3rd, 5th, ... of 2*len elements from stack and reverse it!
-- example) revTakeRhs 2 [a1,a2,a3,a4,a5,a6,...]
--          = [a4, a2]
revTakeRhs :: Int -> [a] -> [a]
revTakeRhs 0 stack = []
revTakeRhs n (_:nt:stack) = revTakeRhs (n-1) stack ++ [nt]
revTakeRhs n stack = error "[revTakeRhs] something wrong happened"

-- Automaton

runAutomaton
  :: (TokenInterface token,
      Typeable token, Typeable ast, Typeable a,
      Show token, Show ast) =>
     Bool -> AutomatonSpec token ast IO a ->
     LexerParserState a -> BoolToLexer IO a token -> IO ast
runAutomaton flag amSpec init_lp_state lexer =
  do maybeConfig <- readConfig
  
     flag <- case maybeConfig of
               Nothing -> return flag
               Just config -> return $ config_DEBUG config

     ST.evalStateT (runYapbAutomaton flag amSpec (lexer flag)) init_lp_state
  
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
  
runYapbAutomaton flag (rm_spec @ 
   AutomatonSpec { 
     am_initState=initState,
     am_actionTbl=actionTbl,
     am_gotoTbl=gotoTbl,
     am_prodRules=prodRules,
     am_parseFuns=pFunList
   }) nextTerminal =

      do let initStack = push (StkState initState) emptyStack
         run Nothing initStack Nothing
  
  where
    {- run :: TokenInterface token => [Terminal token] -> Stack token ast -> IO ast -}
    run maybeTerminal stack _maybeStatus = do
      let state = currentState stack
      -- let terminal = head terminalList

      -- Save the current state in case of going back
      prevState <- ST.get
      
      terminal <-
        case maybeTerminal of
          Nothing -> nextTerminal
          Just t  -> return t      -- Use a terminal multiple times
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
            StkNonterminal (Just ast) _ -> return ast
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

            debug flag ("\t" ++ show prodrule) $

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
                                    throw (NotFoundGoto topState lhs stack
                                             actionTbl gotoTbl prodRules
                                               lp_state maybeStatus)

               let stack2 = push (StkNonterminal (Just ast) lhs) stack1 
               let stack3 = push (StkState toState) stack2
               run (Just terminal) stack3 maybeStatus -- Use the terminal again the next time!

prlevel :: Int -> String
prlevel n = take n (let spaces = ' ' : spaces in spaces)


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
    else do ast <- runAutomaton flag
                     (AutomatonSpec {
                       am_initState=initState,
                       am_actionTbl=actionTbl,
                       am_gotoTbl=gotoTbl,
                       am_prodRules=prodRules,
                       am_parseFuns=pFunList})
                     init_lp_state lexer
            -- putStrLn "done." -- for the interafce with Java-version RPC calculus interpreter.
            return ast

  where
    specFileName      = parserSpecFile parserSpec  -- e.g., mygrammar.grm
    grammarFileName   = grammarFile    parserSpec  --       prod_rules.txt
    actionTblFileName = actionTblFile  parserSpec  --       action_table.txt
    gotoTblFileName   = gotoTblFile    parserSpec  --       goto_table.txt 
    
    sSym          = startSymbol parserSpec
    tokenAttrList = tokenPrecAssoc parserSpec

    tokenAttrs    = toTokenAttrs tokenAttrList
    tokenAttrsStr = show tokenAttrs
    
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
-- | Computing candidates (Todo: This should be separated from the automaton part.)
--------------------------------------------------------------------------------

-- | Candidates
data Candidate = -- data Candidate vs. data EmacsDataItem = ... | Candidate String 
    TerminalSymbol String
  | NonterminalSymbol String
  deriving Eq

data CandidateTree = CandidateTree Candidate [CandidateTree] deriving (Eq,Show)

type CandidateForest = [CandidateTree]

instance Show Candidate where
  showsPrec p (TerminalSymbol s) = (++) $ "Terminal " ++ s
  showsPrec p (NonterminalSymbol s) = (++) $ "Nonterminal " ++ s

leaf :: Candidate -> CandidateTree
leaf cand = CandidateTree cand []

leafs :: CandidateForest -> [Candidate]
leafs []                                     = []
leafs (CandidateTree leaf [] : forest)       = leaf : leafs forest
leafs (CandidateTree leaf subtrees : forest) =
  leafs subtrees ++ leafs forest -- Here, the leaf is not included as a candidate!

toChildren []                          = []
toChildren (StkState i:stk)            = toChildren stk
toChildren (StkTerminal term:stk)      =
  (CandidateTree (TerminalSymbol (terminalToSymbol term)) []) : toChildren stk
toChildren (StkNonterminal ast nt:stk) =
  (CandidateTree (NonterminalSymbol nt) []) : toChildren stk

-- | Automation information
data Automaton token ast =
  Automaton {
    actTbl    :: ActionTable,
    gotoTbl   :: GotoTable,
    prodRules :: ProdRules
  }

-- | Computing candidates
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
     -> IO ([[Candidate]], Bool)

compCandidates ccOption level symbols state stk = 
  let flag = cc_debugFlag ccOption in
  debug flag "" $ 
  debug flag "[compCandidates] " $ 
  debug flag (" - state: " ++ show state) $
  debug flag (" - stack: " ++ prStack stk) $ 
  debug flag "" $ 
  -- compGammasDfs ccOption level symbols state stk []
  do let symbolTrees = map leaf symbols
     (candForestList,bool) <- extendedCompCandidates ccOption symbolTrees state stk
     return (map leafs candForestList, bool)
  
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
  if level > maxLevel then 
    let result_symbols = if null symbols then [] else [symbols] in
    debug flag (prlevel level ++ "maxlevel reached.") $ 
    debug flag (prlevel level ++ " - " ++ show result_symbols) $ 
    do return result_symbols
  else
    checkCycle flag False level state stk "" history
     (\history ->
       {- 1. Reduce -}
       case nub [prnum | ((s,lookahead),Reduce prnum) <- actionTable, state==s, isReducible productionRules prnum stk] of
         [] ->
           {- 2. Goto table -}
           debug flag (prlevel level ++ "no reduce: " ++ show state) $ 
           case nub [(nonterminal,toState) | ((fromState,nonterminal),toState) <- gotoTable, state==fromState] of
             [] -> 
               debug flag (prlevel level ++ "no goto: " ++ show state) $
               {- 3. Accept -}
               if length [True | ((s,lookahead),Accept) <- actionTable, state==s] >= 1
               then 
                    debug flag (prlevel level ++ "accept: " ++ show state) $
                    return []
               {- 4. Shift -}
               else let cand2 = nub [(terminal,snext) | ((s,terminal),Shift snext) <- actionTable, state==s] in
                    let len = length cand2 in
                    case cand2 of
                     [] -> 
                       debug flag (prlevel level ++ "no shift: " ++ show state) $ 
                       return []

                     _  -> do listOfList <-
                                mapM (\ ((terminal,snext),i)-> 
                                   let stk1 = push (StkTerminal (Terminal terminal 0 0 Nothing)) stk in -- Todo: ??? (toToken terminal)
                                   let stk2 = push (StkState snext) stk1 in

                                   debug flag (prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "]: "
                                                ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext) $ 
                                   debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                                   debug flag (prlevel level ++ " - " ++ "Symbols: " ++ show (symbols++[TerminalSymbol terminal])) $ 
                                   debug flag "" $ 

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

                      (\history1 -> 
                       debug flag (prlevel level ++ "GOTO [" ++ show i ++ "/" ++ show len ++ "] at "
                                     ++ show state ++ " -> " ++ show nonterminal ++ " -> " ++ show snext) $ 
                       debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                       debug flag (prlevel level ++ " - " ++ "Symbols:" ++ show (symbols++[NonterminalSymbol nonterminal])) $ 
                       debug flag "" $ 

                       compGammasDfs ccOption (level+1) (symbols++[NonterminalSymbol nonterminal]) snext stk2 history1) )
                         (zip nontermStateList [1..])
               return $ concat listOfList

         prnumList ->
           let len = length prnumList in

           debug flag (prlevel level     ++ "# of prNumList to reduce: " ++ show len ++ " at State " ++ show state) $ 
           debug flag (prlevel (level+1) ++ show [ productionRules !! prnum | prnum <- prnumList ]) $ 

           do listOfList <-
               mapM (\ (prnum,i) -> (
                 -- checkCycle False level state stk ("REDUCE " ++ show prnum) history
                 checkCycle flag True level state stk (show prnum) history
                   (\history1 -> 
                      debug flag (prlevel level ++ "REDUCE"
                                      ++ " [" ++ show i ++ "/" ++ show len ++ "]") $ 
                      debug flag (prlevel level ++ " - prod rule: " ++ show (productionRules !! prnum)) $ 
                      debug flag (prlevel level ++ " - State " ++ show state) $ 
                      debug flag (prlevel level ++ " - Stack " ++ prStack stk) $ 
                      debug flag (prlevel level ++ " - Symbols: " ++ show symbols) $ 
                      debug flag "" $ 
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
  then
    debug flag (prlevel level ++ "[LEN COND: False] length rhs > length symbols: NOT "
                   ++ show rhsLength ++ ">" ++ show (length symbols)) $ 
    debug flag ("rhs: " ++ show rhs) $ 
    debug flag ("symbols: " ++ show symbols) $ 
    return [] -- Todo: (if null symbols then [] else [symbols])
    
  else    -- reducible
    let stk1 = drop (rhsLength*2) stk in
    let topState = currentState stk1 in
    let toState =
         case lookupGotoTable (gotoTbl automaton) topState lhs of
           Just state -> state
           Nothing -> error $ "[compGammasDfsForReduce] Must not happen: lhs: "
                                ++ lhs ++ " state: " ++ show topState
    in                            
    let stk2 = push (StkNonterminal Nothing lhs) stk1 in -- ast
    let stk3 = push (StkState toState) stk2 in
    debug flag (prlevel level ++ " - GOTO : "
                   ++ show topState ++ " " ++ lhs ++ " " ++ show toState) $ 
    debug flag (prlevel level ++ " --- Stack " ++ prStack stk3) $ 
    debug flag "" $ 

    debug flag (prlevel level ++ "Found a gamma: " ++ show symbols) $ 
    debug flag "" $ 

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
type State           = Int
type LengthOfSymbols = Int
  
--
extendedCompCandidates
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [CandidateTree] -> State -> Stack token ast -> IO ([[CandidateTree]], Bool)
extendedCompCandidates ccOption symbols state stk = do
  -- check yapb.config
  maybeConfig <- readConfig
  case maybeConfig of
    Nothing ->
      do list <- extendedCompCandidates' ccOption symbols state stk
         return (map (\(a,b,c)->c) list, True)
         
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
                      
      in
      do list <- extendedCompCandidates' ccOption' symbols state stk
         return (map (\(a,b,c)->c) list, display)

  where
    -- main function
    extendedCompCandidates' ccOption symbols state stk =
      let
         debugFlag = cc_debugFlag ccOption
         isSimple  = cc_simpleOrNested ccOption
         r_level   = cc_r_level ccOption
         gs_level  = cc_gs_level ccOption
      in
         debug debugFlag ("simple/nested(True/False): " ++ show isSimple) $ 
         debug debugFlag ("(Max) r level: " ++ show r_level) $ 
         debug debugFlag ("(Max) gs level: " ++ show gs_level) $ 
         debug debugFlag "" $ 

         -- do list <- if isSimple
         --              then do repReduce ccOption symbols state stk
         --              else do extendedNestedCandidates ccOption [(state, stk, symbols)]

         --    return [ c | (state, stk, c) <- list, null c == False ]
            
         do if isSimple
              then do repReduce ccOption symbols state stk
              else do extendedNestedCandidates ccOption [(state, stk, symbols)]


-- Extended simple candidates
-- extendedSimpleCandidates
--     :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
--        CompCandidates token ast -> State -> Stack token ast -> IO [(State, Stack token ast, [CandidateTree])]
       
-- extendedSimpleCandidates ccOption state stk = repReduce ccOption [] state stk 


-- Extended nested candidates
extendedNestedCandidates
    :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
       CompCandidates token ast -> [(State, Stack token ast, [CandidateTree])]
       -> IO [(State, Stack token ast, [CandidateTree])]
       
extendedNestedCandidates ccOption initStateStkCandsList =
  let debugFlag = cc_debugFlag ccOption
      r_level   = cc_r_level ccOption

      level     = cc_printLevel ccOption

      len       = length initStateStkCandsList

      f ((state, stk, symbols),i) =
          debug debugFlag (prlevel level ++ "[extendedNestedCandidates] : " ++ show i ++ "/" ++ show len) $ 
          debug debugFlag (prlevel level ++ " - state " ++ show state) $ 
          debug debugFlag (prlevel level ++ " - stack " ++ prStack stk) $ 
          debug debugFlag (prlevel level ++ " - symbs " ++ show symbols) $ 
          debug debugFlag "" $ 

          do list <- repReduce ccOption{cc_simpleOrNested=True,cc_printLevel=level+1} [] state stk
             return [ (state,stk,symbols++cands) | (state,stk,cands) <- list]
  in
  if r_level > 0
  then
    do stateStkCandsListList <- mapM f (zip initStateStkCandsList [1..])

       -- if null stateStkCandsListList
       --   then return initStateStkCandsList
       --   else do nextStateStkCandsList <-
       --             extendedNestedCandidates ccOption{cc_r_level=r_level-1}
       --                [ (toState, toStk, {- fromCand ++ -} toCand)

       --                | ((fromState, fromStk, fromCand), toList)
       --                    <- zip initStateStkCandsList stateStkCandsListList

       --                , (toState, toStk, toCand) <- toList
       --                ]

       --           return $ {- initStateStkCandsList ++ -} nextStateStkCandsList
                 
       extendedNestedCandidates ccOption{cc_r_level=r_level-1} (concat stateStkCandsListList)

  else
    return initStateStkCandsList  -- cf. []

repReduce
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [CandidateTree] -> State -> Stack token ast
     -> IO [(State, Stack token ast, [CandidateTree])]

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
     then 
          debug flag (prlevel level ++ "accept: " ++ show state) $ 
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

simulReduce :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [CandidateTree]
 -> Int  -- Production rule number
 -> Int  -- of all reducible actions
 -> Int  -- ith action chosen
 -> State
 -> Stack token ast
 -> IO [(State, Stack token ast, [CandidateTree])]
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
     -- debug flag $ prlevel level ++ "[simulReduce] " ++ show (cc_searchState ccOption)

     debug flag (prlevel level ++ "REDUCE [" ++ show i ++ "/" ++ show len ++ "] at ") $ 
     debug flag (prlevel level ++ " - prod rule: " ++ show (productionRules !! prnum)) $ 
     debug flag (prlevel level ++ " - State " ++ show state) $ 
     debug flag (prlevel level ++ " - Stack " ++ prStack stk) $ 
     debug flag (prlevel level ++ " - Symbols: " ++ show symbols) $ 
     debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
     debug flag "" $ 

     if rhsLength > length symbols && length symbols > 0   -- This is the time to stop!
     then
       if isFinalReduce searchState
       then
         debug flag (prlevel level ++ "rhsLength > length symbols: final") $
         debug flag "" $
         
         do let stk1     = drop (rhsLength*2) stk
            -- let children = toChildren $ reverse $ take (rhsLength*2) stk
            -- [CandidateTree (NonterminalSymbol lhs) children]
            let topState = currentState stk1
            let toState  = case lookupGotoTable (gotoTbl automaton) topState lhs of
                  Just state -> state
                  Nothing -> error $ "[simulReduce] Must not happen: lhs: "
                                     ++ lhs ++ " state: " ++ show topState
            let stk2 = push (StkNonterminal Nothing lhs) stk1  -- ast
            let stk3 = push (StkState toState) stk2
            return [(toState, stk3, symbols)]  -- Note: toState and stk3 are after the reduction, but symbols are not!! 
            
       else
         repGotoOrShift
           (ccOption{cc_searchState =
                     SS_GotoOrShift
                     (r_level (cc_searchState ccOption))
                     (gs_level (cc_searchState ccOption)) })
             symbols state stk

     -- rhsLength <= length symbols || length symbols == 0
     else do let stk1 = drop (rhsLength*2) stk
             let topState = currentState stk1
             let toState = case lookupGotoTable (gotoTbl automaton) topState lhs of
                   Just state -> state
                   Nothing -> error $ "[simulReduce] Must not happen: lhs: "
                                      ++ lhs ++ " state: " ++ show topState
             let stk2 = push (StkNonterminal Nothing lhs) stk1  -- ast
             let stk3 = push (StkState toState) stk2

             let reducedSymbols =
                   if rhsLength <= length symbols
                   then let revSymbols = reverse symbols
                            children   = reverse (take rhsLength revSymbols)
                            therest    = drop rhsLength $ revSymbols
                        in  reverse $ (CandidateTree (NonterminalSymbol lhs) children :) $ therest
                   else symbols

             if isSimple then  -- simple mode

               if isInitReduces searchState then -- reduces until symbols are found
                 do -- listOfList <- repReduce ccOption{cc_printLevel=level+1} reducedSymbols toState stk3
                    repReduce ccOption{cc_printLevel=level+1} reducedSymbols toState stk3

                    -- let f syms0 (s, stk, syms) = (s, stk, syms0 ++ syms)

                    -- return (if null symbols
                    --         then listOfList
                    --         else {- (toState, stk3, symbols) : -} map (f symbols) listOfList)  -- Q: symbols: 필요?
                    
               else if isFinalReduce searchState then  -- Todo: isFinalReduce???
                 do repReduce ccOption{cc_printLevel=level+1} reducedSymbols toState stk3 -- Just copied the code above!
                    
                 -- do return (if null symbols
                 --            then []
                 --            else [(toState, stk3, symbols)])
                    
               else -- SS_GotoOrShift
                 do error $ "simulReduce: Unexpected search state" ++ show searchState

             else -- nested mode
               do error $ "simulReduce: Unexpected nested mode: "


simulGoto :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [CandidateTree]
 -> Int
 -> [StkElem token ast]
 -> IO [(State, Stack token ast, [CandidateTree])]
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
                mapM (\ ((nonterminal,snext),i) -> 
                          let stk1 = push (StkNonterminal Nothing nonterminal) stk in
                          let stk2 = push (StkState snext) stk1 in

                          debug flag (prlevel level ++ "GOTO [" ++ show i ++ "/" ++ show len ++ "] at "
                                         ++ show state ++ " -> " ++ show nonterminal ++ " -> " ++ show snext) $ 
                          debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                          debug flag (prlevel level ++ " - " ++ "Symbols:" ++ show (symbols++[CandidateTree (NonterminalSymbol nonterminal) []])) $ 
                          debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
                          debug flag "" $ 

                          repGotoOrShift 
                            ccOption{cc_printLevel=level+1}
                              (symbols++[CandidateTree (NonterminalSymbol nonterminal) []])
                                snext stk2)
                  (zip nontermStateList [1..])

              return $ concat listOfList

simulShift :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [CandidateTree]
 -> Int
 -> [StkElem token ast]
 -> IO [(State, Stack token ast, [CandidateTree])]
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
                    mapM (\ ((terminal,snext),i)-> 
                              let stk1 = push (StkTerminal (Terminal terminal 0 0 Nothing)) stk in
                              let stk2 = push (StkState snext) stk1 in

                              debug flag (prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "]: "
                                            ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext) $ 
                              debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                              debug flag (prlevel level ++ " - " ++ "Symbols: " ++ show (symbols++[CandidateTree (TerminalSymbol terminal) []])) $ 
                              debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
                              debug flag "" $ 

                              repGotoOrShift
                                ccOption{cc_printLevel=level+1}
                                  (symbols++[CandidateTree (TerminalSymbol terminal) []])
                                    snext stk2)
                      (zip cand2 [1..])

                  return $ concat listOfList

repGotoOrShift
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [CandidateTree] -> State -> Stack token ast -> IO [(State, Stack token ast, [CandidateTree])]

repGotoOrShift ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in
     -- debug flag (prlevel level ++ "[repGotoOrShift] " ++ show (cc_searchState ccOption)) $

     do if null [True | ((s,lookahead),Accept) <- actionTable, state==s] == False
        then 
             debug flag (prlevel level ++ "accept: " ++ show state) $ 
             return []

        else do
                listOfList1 <- repReduce
                                 (ccOption{cc_searchState=
                                             SS_FinalReduce
                                               (r_level (cc_searchState ccOption))
                                               (gs_level (cc_searchState ccOption))})
                                   symbols state stk

                if null listOfList1 -- \|\| isInitReduces (cc_searchState ccOption)
                  then
                       -- let listOfList1 = []

                       if gs_level (cc_searchState ccOption) - 1 > 0 then
                         let ccOption' = ccOption{cc_searchState=
                                             SS_GotoOrShift
                                               (r_level (cc_searchState ccOption))
                                               (gs_level (cc_searchState ccOption) - 1)}
                         in

                         -- both goto and shift only once 
                         if gs_level (cc_searchState ccOption) == cc_gs_level ccOption
                         then

                           do listOfList2 <- simulGoto ccOption' symbols state stk
                              listOfList3 <- simulShift ccOption' symbols state stk

                              debug flag (prlevel level ++ "[repGotoOrShift] (1) final") $
                               debug flag "" $
                                if null (listOfList1 ++ listOfList2 ++ listOfList3)
                                then return []
                                else return (listOfList1 ++ listOfList2 ++ listOfList3)

                         else

                           do listOfList2 <- simulGoto ccOption' symbols state stk

                              if null listOfList2
                              then

                                do listOfList3 <- simulShift ccOption' symbols state stk

                                   debug flag (prlevel level ++ "[repGotoOrShift] (2) final") $
                                    debug flag "" $
                                     if null (listOfList1 ++ listOfList2 ++ listOfList3)
                                     then return []
                                     else return (listOfList1 ++ listOfList2 ++ listOfList3)

                              else
                                debug flag (prlevel level ++ "[repGotoOrShift] (3): final") $
                                 debug flag "" $
                                  if null (listOfList1 ++ listOfList2)
                                  then return []
                                  else return (listOfList1 ++ listOfList2)

                       else
                         debug flag (prlevel level ++ "gs_level == 0: final") $
                         debug flag (prlevel level) $
                         do return []

                  else do return listOfList1

-- Todo: repReduce를 하지 않고
--       Reduce 액션이 있는지 보고
--       없으면 goto or shift 진행하고
--       있으면 reduce한번하고 종료!

--       현재 구현은 repReduce 결과가 널인지 검사해서 진행 또는 종료
--       Reduce 액션이 있어도 진행될 수 있음!

--
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
  then 
    debug debugflag (prlevel level ++ "CYCLE is detected !!") $ 
    debug debugflag (prlevel level ++ " - " ++ show state ++ " " ++ action) $ 
    debug debugflag (prlevel level ++ " - " ++ prStack stk) $ 
    debug debugflag "" $ 
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
    nonterminalToStringMaybe :: Maybe (String->String),
    presentation :: Int  -- 0: default, no transformation. 1: a list of the first symbols
  }

defaultHandleParseError = HandleParseError {
    debugFlag = False,
    searchMaxLevel = 1,
    simpleOrNested = True,
    postTerminalList = [],
    nonterminalToStringMaybe = Nothing,
    presentation = 0
  }

-- | handleParseError
  
handleParseError :: TokenInterface token => HandleParseError token -> ParseError token ast a -> IO [EmacsDataItem]
handleParseError hpeOption parseError =
  do maybeConfig <- readConfig
  
     let hpeOption' =
           case maybeConfig of
             Nothing     -> hpeOption
             Just config -> hpeOption{debugFlag = config_DEBUG config
                                     ,presentation = config_PRESENTATION config}

     unwrapParseError hpeOption' parseError

  --
unwrapParseError hpeOption (NotFoundAction _ state stk _actTbl _gotoTbl _prodRules lp_state maybeStatus) = do
    let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules}
    arrivedAtTheEndOfSymbol hpeOption state stk automaton lp_state maybeStatus
    
unwrapParseError hpeOption (NotFoundGoto state _ stk _actTbl _gotoTbl _prodRules lp_state maybeStatus) = do
    let automaton = Automaton {actTbl=_actTbl, gotoTbl=_gotoTbl, prodRules=_prodRules}
    arrivedAtTheEndOfSymbol hpeOption state stk automaton lp_state maybeStatus

--
arrivedAtTheEndOfSymbol hpeOption state stk automaton lp_state@(_,_,_,"") maybeStatus =  -- [$]
  case maybeStatus of
    Nothing ->
      debug (debugFlag hpeOption) "No saved stack" $ 
      _handleParseError hpeOption state stk automaton
    Just savedStk ->
      let savedStk = fromJust maybeStatus
          savedState = currentState savedStk
      in 
        debug (debugFlag hpeOption) "Restored stack" $ 
        debug (debugFlag hpeOption) (" - state: " ++ show savedState) $ 
        debug (debugFlag hpeOption) (" - stack: " ++ prStack savedStk) $ 
        _handleParseError hpeOption savedState savedStk automaton
              
arrivedAtTheEndOfSymbol hpeOption state stk automaton lp_state@(_,_,_,text) maybeStatus =
     -- debug (debugFlag hpeOption) $ "length terminalList /= 1 : " ++ show (length terminalList)
     -- debug (debugFlag hpeOption) $ map (\t -> terminalToString $ t) terminalList
     return [SynCompInterface.ParseError text]

_handleParseError
  (hpeOption @ HandleParseError {
      debugFlag=flag,
      searchMaxLevel=maxLevel,
      simpleOrNested=isSimple,
      postTerminalList=terminalListAfterCursor,
      nonterminalToStringMaybe=_nonterminalToStringMaybe,
      presentation=howtopresent})
  state stk automaton = 
  let ccOption = CompCandidates {
        cc_debugFlag=flag,
        cc_printLevel=0,
        cc_maxLevel=maxLevel,
        cc_simpleOrNested=isSimple,
        cc_automaton=automaton,
        cc_searchState = initSearchState init_r_level init_gs_level,
        cc_r_level = init_r_level,  
        cc_gs_level = init_gs_level}
  in
  let convFun =
        case _nonterminalToStringMaybe of
          Nothing -> \s -> "..."
          Just fn -> fn
  in
  do (candidateListList, emacsDisplay) <- compCandidates ccOption 0 [] state stk
     let colorListList_symbols =
          [ filterCandidates candidateList terminalListAfterCursor
          | candidateList <- candidateListList ]
          
     let colorListList_ = map (stringfyCandidates convFun) colorListList_symbols 
     let colorListList = map collapseCandidates colorListList_ 
     let emacsColorListList  = map (map showEmacsColor) colorListList 
     let strList = nub [ concatStrList strList | strList <- emacsColorListList ] 
     let rawStrListList = nub [ strList | strList <- map (map showRawEmacsColor) colorListList ]

     debug (flag || True) "" $ 
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
multiDbg [] = \x -> x
multiDbg (f:fs) = f . multiDbg fs
    
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
debug :: Bool -> String -> a -> a
debug flag msg x = if flag then trace msg x else x

