module LoadAutomaton where

import AutomatonType
import SaveProdRules(tokenizeLhs)
import System.IO

loadAutomaton :: String -> String -> String
              -> IO (ActionTable, GotoTable, ProdRules)
loadAutomaton grammarFileName actionTblFileName gotoTblFileName = do
  grammarStr   <- readFile grammarFileName
  actionTblStr <- readFile actionTblFileName
  gotoTblStr   <- readFile gotoTblFileName

  actionTbl <- loadActionTbl actionTblStr
  gotoTbl   <- loadGotoTbl gotoTblStr
  prodRules <- loadProdRules grammarStr

  return (actionTbl, gotoTbl, prodRules)

-- Load action table
loadActionTbl :: String -> IO ActionTable
loadActionTbl str = tokenizeStateNumInAction str

tokenizeStateNumInAction :: String -> IO ActionTable
tokenizeStateNumInAction str =   
  case lex str of
    [] -> return []
    [("", therest)] -> return []
    [(stateNum, therest)] -> do
      (terminal, action, actTbl) <- tokenizeTerminalInAction therest
      return $ ((read stateNum :: Int, terminal), action) : actTbl

tokenizeTerminalInAction :: String -> IO (String, Action, ActionTable)
tokenizeTerminalInAction str =
  case lex str of
    [] -> fail "No terminal found (1)"
    [("", therest)] -> fail "No terminal found (2)"
    [(terminal, therest)] -> do
      (action, actTbl) <- tokenizeActioninAction therest
      return (terminal, action, actTbl)

tokenizeActioninAction :: String -> IO (Action, ActionTable)
tokenizeActioninAction str =
  case lex str of
    [] -> fail "No action found (1)"
    [("", therest)] -> fail "No action found (2)"
    [(action, therest)] -> do
      case action of
        "Shift" -> do
          tokenizeShiftReduceStateNumInAction therest Shift
        "Reduce" -> do
          tokenizeShiftReduceStateNumInAction therest Reduce
        "Accept" -> do
          actTbl <- tokenizeStateNumInAction therest
          return (Accept, actTbl)

tokenizeShiftReduceStateNumInAction :: String -> (Int -> Action)
  -> IO (Action, ActionTable)
tokenizeShiftReduceStateNumInAction str fn =
  case lex str of
    [] -> fail "No shift/reduce state number found (1)"
    [("", therest)] -> fail "No shift/reduce state number found (2)"
    [(stateNum, therest)] -> do
      actTbl <- tokenizeStateNumInAction therest
      return (fn (read stateNum :: Int), actTbl)
      

-- Load goto table
loadGotoTbl :: String -> IO GotoTable
loadGotoTbl str = tokenizeStateNumInGoto str

tokenizeStateNumInGoto :: String -> IO GotoTable
tokenizeStateNumInGoto str =
  case lex str of
    [] -> return []
    [("", therest)] -> return []
    [(stateNum, therest)] -> do
      (nonterminal, toStateNum, actTbl) <- tokenizeNonterminalInGoto therest
      return $ ((read stateNum :: Int, nonterminal), read toStateNum :: Int) : actTbl

tokenizeNonterminalInGoto :: String -> IO (String, String, GotoTable)
tokenizeNonterminalInGoto str =
  case lex str of
    [] -> fail "No nonterminal found (1)"
    [("", therest)] -> fail "No nonterminal found (2)"
    [(nonterminal,therest)] -> do
      (toStateNum, actTbl) <- tokenizeToStateNumInGoto therest
      return (nonterminal, toStateNum, actTbl)

tokenizeToStateNumInGoto :: String -> IO (String, GotoTable)
tokenizeToStateNumInGoto str =
  case lex str of
    [] -> fail "No to-state found (1)"
    [("", therest)] -> fail "No to-state found (2)"
    [(toStateNum,therest)] -> do
      actTbl <- tokenizeStateNumInGoto therest
      return (toStateNum, actTbl)

-- Load production rules
loadProdRules :: String -> IO ProdRules
loadProdRules str = do
  numLhsRhsList <- mapM tokenizeNumInProdRules (splitWithCR str)
  return [ (lhs, rhs) | (i, lhs, rhs) <- numLhsRhsList ]

tokenizeNumInProdRules :: String -> IO (Int, String, [String])
tokenizeNumInProdRules str =
  case lex str of 
    [] -> fail "No rule number found (1)"
    [("", therest)] -> fail "No rule number found (2)"
    [(ruleNumStr, therest)] -> do
      (lhs, rhs) <- tokenizeColonInProdRules therest
      return (read ruleNumStr :: Int, lhs, rhs)

tokenizeColonInProdRules :: String -> IO (String, [String])
tokenizeColonInProdRules str =
  case lex str of
    [] -> fail "No colon found (1)"
    [("", therest)] -> fail "No colon found (2)"
    [(colon, therest)] -> do
      let lhsRhs = tokenizeLhs therest
      return (head lhsRhs, tail lhsRhs)
    

splitWithCR :: String -> [String]
splitWithCR str =
  [ line | line <- splitWithCR' "" str, line /= "" ]

splitWithCR' :: String -> String -> [String]
splitWithCR' app [] = (reverse app) : []
splitWithCR' app ('\n':therest) = (reverse app) : splitWithCR' "" therest
splitWithCR' app (ch:therest) = splitWithCR' (ch : app) therest
      
