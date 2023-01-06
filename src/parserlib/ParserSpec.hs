module ParserSpec where

import Attrs
import AutomatonStack

import qualified Control.Monad.Trans.State.Lazy as ST

import TokenInterface
import Terminal

--------------------------------------------------------------------------------
-- | Lexer
--------------------------------------------------------------------------------

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
               genparserexe   :: String,   -- ex) genlrparse-exe
               synCompSpec    :: Maybe SynCompSpec,
               parserTime     :: ParserTime m a
             }

data ParserTime m a =
  ParserTime {
    pa_startTime  :: ST.StateT (LexerParserState a) m Integer,
    pa_finishTime :: Integer -> ST.StateT (LexerParserState a) m ()
  }

data SynCompSpec =
  SynCompSpec { isAbleToSearch :: String -> Bool   -- terminasls or non-terminals
              }

-- -- Specification
-- data Spec token ast =
--   Spec (LexerSpec token) (ParserSpec token ast)


