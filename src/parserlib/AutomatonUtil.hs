module AutomatonUtil where
    
import TokenInterface
import Terminal
import AutomatonStack
import AutomatonType

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

-- |

takeRet n [] = ""
takeRet 0 ('\n':text) = '\n' : takeRet 0 text
takeRet n ('\n':text)  = ""
takeRet n (c:text) = c : (takeRet (n+1) text)