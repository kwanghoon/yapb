{-# LANGUAGE GADTs #-}


module ParseError where

import AutomatonType
import AutomatonStack
import ParserSpec
import TokenInterface
import Terminal
import AutomatonUtil

import Control.Exception
import Data.Typeable

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

lpStateFrom :: ParseError token ast a -> (a, Line, Column, String)
lpStateFrom (NotFoundAction _ _ _ _ _ _ lpstate _) = lpstate
lpStateFrom (NotFoundGoto   _ _ _ _ _ _ lpstate _) = lpstate