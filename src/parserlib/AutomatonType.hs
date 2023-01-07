module AutomatonType where

import ParserSpec ( ParseAction, LexerParserState )
import qualified Control.Monad.Trans.State.Lazy as ST

--------------------------------------------------------------------------------
-- | Parser
--------------------------------------------------------------------------------

data Action = Shift Int | Reduce Int | Accept deriving (Eq, Show)

type ActionTable = [((Int, String), Action)] -- key: (Int,String), value: Action
type GotoTable   = [((Int, String), Int)]    -- key: (Int,String), value: Int
type ProdRules   = [(String, [String])]      -- key: Int,          value: (String, [String])

prProdRule :: (String, [String]) -> String
prProdRule (x,ys) =  x ++ " -> "  ++ pr_ys ys
  where
    pr_ys []     = ""  
    pr_ys [y]    = y 
    pr_ys (y:ys) = y ++ " " ++ pr_ys ys

--------------------------------------------------------------------------------
-- | Automation specification
--------------------------------------------------------------------------------

type ParseActionList token ast m a = [ParseAction token ast m a]

data AutomatonSpec token ast m a =
  AutomatonSpec {
    am_actionTbl   :: ActionTable,
    am_gotoTbl     :: GotoTable,
    am_prodRules   :: ProdRules,
    am_parseFuns   :: ParseActionList token ast m a,
    am_initState   :: Int,
    am_time        :: AutomatonTime m a
  }

data AutomatonTime m a =
  AutomatonTime {
    am_startTime  :: ST.StateT (LexerParserState a) m Integer,
    am_finishTime :: Integer -> ST.StateT (LexerParserState a) m (),
    am_cputime    :: Integer
  }