module ActionLogType where

import Terminal ( Terminal, terminalToTokenSymbol )
import TokenInterface ( TokenInterface )

-- | Action logs

type State = Int 
type ProdRuleNumber = Int 
type Nonterminal = String 
type ProdRuleText = String 
type LengthOfRHS = Int

data ActionLog token = 
    LogShift State (Terminal token) 
  | LogReduce ProdRuleNumber ProdRuleText LengthOfRHS
  | LogGoto State Nonterminal
  | LogAccept

type ActionLogs token = [ ActionLog token ]

showActionLog :: TokenInterface token => ActionLog token -> String
showActionLog (LogShift i t) = "Shift " ++ show i ++ " " ++ terminalToTokenSymbol t
showActionLog (LogReduce i p _) = "Reduce " ++ show i ++ " " ++ p
showActionLog (LogGoto i nt) = "Goto " ++ show i ++ " " ++ nt
showActionLog LogAccept = "Accept"
