module AutomatonStack where

import TokenInterface
import Terminal

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

