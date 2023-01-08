module AnalyzeActions ( analyzeActions ) where

import ActionLogType
import SynCompAlgoUtil
import TokenInterface ( TokenInterface )
import Terminal ( terminalToTokenSymbol )
import Control.Monad.RWS (MonadState(state))
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Bifunctor

analyzeActions :: TokenInterface token => ActionLogs token -> IO ()
analyzeActions logs =
    let map = collect logs in
        mapM_ (print . Data.Bifunctor.second Map.toList) (Map.toList map)

collect :: TokenInterface token => ActionLogs token -> Map.Map State ( Map.Map CandidateForest Integer )
collect logs =
    let initState = 0
        map = onLogs initState logs Map.empty
    in  map


onLogs :: TokenInterface token => State -- ^ 
  -> [ActionLog token] -- ^ 
  -> Map.Map State ( Map.Map CandidateForest Integer ) -- ^ state |-> { candidate1 |-> n1, ..., candidatek |-> nk }
  -> Map.Map State ( Map.Map CandidateForest Integer )
onLogs state [] map = map
onLogs state logs map =
    let candidate = initRepReduce state logs
        (maybeNextState, nextLogs) = getNextState logs

        foundMap = Map.fromList [ (candidate,1) ]

        f newMap oldMap = 
          let (v,c) = head (Map.toList newMap) in
            Map.insertWith g v c oldMap
        g nc oc = oc + nc

        newMap = if null candidate then map
                 else Map.insertWith f state foundMap map
    in
        case maybeNextState of
            Just nextState -> onLogs nextState nextLogs newMap
            Nothing -> newMap

getNextState :: [ActionLog token] -> (Maybe State, [ActionLog token])
getNextState (LogShift state terminal : logs) = (Just state, logs)
-- getNextState (LogGoto state nonterminal : LogReduce {} : logs) = getNextState logs -- Skip repetitive Reduce and Goto
getNextState (LogGoto state nonterminal : logs) = (Just state, logs)
getNextState (LogAccept : logs) = (Nothing, logs)
getNextState (LogReduce prodRuleNum prodRuleText _ : logs) = getNextState logs
getNextState [] = (Nothing, [])


initRepReduce :: TokenInterface token => State -> [ActionLog token] -> CandidateForest
initRepReduce currentState
    (LogReduce prodRuleNum prodRuleText _ : LogGoto state nonTerminal : logs)
        = initRepReduce state logs

initRepReduce currentState
    (LogReduce prodRuleNum prodRuleText _ : logs)
        = error $ "[initRepReduce] Not Goto after Reduce: "
                    ++ if null logs then "[]" else showActionLog (head logs)
                    ++ " at state " ++ show currentState

initRepReduce currentState logs = repShiftOrGoto currentState [] logs


repShiftOrGoto :: TokenInterface token => State -> CandidateForest -> [ActionLog token] -> CandidateForest
repShiftOrGoto currentState symTrees
    (LogShift state terminal : logs) =
        repShiftOrGoto state (symTrees ++ [Leaf (TerminalSymbol (terminalToTokenSymbol terminal))]) logs

repShiftOrGoto currentState symTrees
    (LogReduce prodRuleNum prodRuleText rhsLength : LogGoto state nonterminal : logs) =
        if length symTrees <= rhsLength then symTrees
        else repShiftOrGoto state
                (simulReduce symTrees rhsLength [Leaf (NonterminalSymbol nonterminal)]) logs

repShiftOrGoto currentState symTrees (LogReduce prodRuleNum prodRuleText _ : logs)
    = error $ "[repShiftOrGoto] Not Goto after Reduce: "
                    ++ if null logs then "[]" else showActionLog (head logs)
                    ++ " at state " ++ show currentState

repShiftOrGoto currentState symTrees (LogAccept : logs) = []

repShiftOrGoto currentState symTrees logs
    =  error $ "[repShiftOrGoto] Unexpected logs: "
                    ++ if null logs then "[]" else showActionLog (head logs)
                    ++ " at state " ++ show currentState

-- | simulReduce is only called from repShiftOrGoto when length symTrees > rhsLength !!
simulReduce symTrees rhsLength leafWithNonterminal =
    reverse (drop rhsLength (reverse symTrees)) ++ leafWithNonterminal