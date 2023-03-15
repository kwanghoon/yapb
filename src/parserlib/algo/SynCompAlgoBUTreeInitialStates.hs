module SynCompAlgoBUTreeInitialStates (compCandidates) where

import AutomatonType
import Terminal
import TokenInterface
import CommonParserUtil
import SynCompAlgoUtil
import Config

import Data.Typeable
import Data.List (nub)

--
compCandidates
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast
     -> Int
     -> [Candidate]
     -> Int
     -> Stack token ast
     -> IO ([[Candidate]], Bool)

compCandidates ccOption level symbols state stk = 
  do let symbolTrees = map candidateLeaf symbols
     stateList <- extendedCompCandidates ccOption symbolTrees state stk
     putStrLn (show stateList)
     return ([map (TerminalSymbol . show) stateList], True)

--------------------------------------------------------------------------------
-- A new search algorithm
--------------------------------------------------------------------------------
--
type State           = Int
type LengthOfSymbols = Int


--
extendedCompCandidates
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast 
     -> [CandidateTree]
     -> State 
     -> Stack token ast 
     -> IO [State]
extendedCompCandidates ccOption symbols state stk = do
  maybeConfig <- readConfig

  let debugFlag =
        case maybeConfig of
          Nothing -> cc_debugFlag ccOption
          Just config -> config_DEBUG config

  let automaton  = cc_automaton ccOption

  stateList <- repReduce debugFlag automaton symbols state stk
  return stateList

repReduce
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     Bool 
     -> Automaton token ast 
     -> [CandidateTree]
     -> State 
     -> Stack token ast
     -> IO [State]

repReduce flag automaton symbols state stk =
  let actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in 
      case nub [prnum | ((s,lookahead),Reduce prnum) <- actionTable
                        , state==s
                        , isReducible productionRules prnum stk] of
          []        -> return [state] 

          prnumList -> 
            do listOfList <- mapM (simulReduce flag automaton symbols state stk) prnumList
               return $ state : concat listOfList

simulReduce :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
  Bool 
  -> Automaton token ast
  -> [CandidateTree] 
  -> State
  -> Stack token ast
  -> Int  -- Production rule number
  -> IO [State]
simulReduce flag automaton symbols state stk prnum =
  let productionRules = prodRules automaton
      prodrule  = (prodRules automaton) !! prnum
      lhs       = fst prodrule
      rhs       = snd prodrule
      
      rhsLength = length rhs
  in
    -- debug flag (prlevel level ++ "REDUCE " ++ 
    --             showProductionRule (productionRules !! prnum))   $ 
    -- debug flag (prlevel level ++ " - prod rule: " ++ show (productionRules !! prnum)) $ 
    -- debug flag (prlevel level ++ " - State " ++ show state) $ 
    -- debug flag (prlevel level ++ " - Stack " ++ prStack stk) $ 
    -- debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
    -- debug flag "" $ 

    do 
      let stk1 = drop (rhsLength*2) stk
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
                 in  reverse $ (candidateNode (NonterminalSymbol lhs) children :) $ therest
            else symbols

      repReduce flag automaton reducedSymbols toState stk3


