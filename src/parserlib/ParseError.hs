{-# LANGUAGE GADTs #-}


module ParseError where

import AutomatonType ( ActionTable, GotoTable, ProdRules, AutomatonState )
import AutomatonStack ( Stack, StkElem )
import ParserSpec ( Column, LexerParserState, Line )
import TokenInterface ( TokenInterface )
import Terminal ( terminalToString, Terminal )
import AutomatonUtil ( takeRet )

import Control.Exception ( Exception )
import Data.Typeable ( Typeable )

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
      Terminal token -> CurrentState -> Stack token ast ->
      ActionTable -> GotoTable -> ProdRules ->
      LexerParserState a ->  -- [Terminal token]
      Maybe [StkElem token ast] ->
      ParseError token ast a

    -- topState, lhs, stack, actiontbl, gototbl,
    NotFoundGoto ::
      (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
      StateOnStackTop -> LhsSymbol -> Stack token ast ->
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

lpStateFrom :: ParseError token ast a -> ((a, Line, Column, String), AutomatonState)
lpStateFrom (NotFoundAction _ currentState _ _ _ _ lpstate _) = (lpstate, currentState)
lpStateFrom (NotFoundGoto   stateOnStackTop _ _ _ _ _ lpstate _) = (lpstate, stateOnStackTop)