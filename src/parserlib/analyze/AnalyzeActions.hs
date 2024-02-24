{-# LANGUAGE GADTs #-}

module AnalyzeActions ( analyzeActions ) where

import ActionLogType
-- import SynCompAlgoUtil
import TokenInterface ( TokenInterface )
import Terminal ( Terminal(..), terminalToTokenSymbol )
import Control.Monad.RWS (MonadState(state))
import Data.Maybe
-- import qualified Data.Map as Map
import qualified Data.Bifunctor
import Test.Hspec (xcontext)

import Debug.Trace (trace)

-- | SynCompAlgoUtil

-- | Candidates
data Candidate token where
    TerminalSymbol :: TokenInterface token => Terminal token -> Candidate token
    NonterminalSymbol :: TokenInterface token => String -> Candidate token

-- | Candidate tree

-- data CandidateTree = CandidateTree Candidate [CandidateTree] deriving (Eq,Show)

{-
--   ( CandidateTree cand [] )^*          ===> Leaf cand
--   ( CandidateTree cand [t1,...,tn] )^* ===> Node cand [t1^*, ..., tn^*]
--
--   A ->                ===> Node (Nonterminal "A") []
--                            is not equal to Leaf (Nonterminal "A").
-}

data CandidateTree token where
    Leaf :: TokenInterface token => Candidate token -> CandidateTree token
    Node :: TokenInterface token => Candidate token -> [CandidateTree token] -> CandidateTree token

-- type CandidateForest = [CandidateTree]

instance TokenInterface token => Show (Candidate token) where
  showsPrec p (TerminalSymbol tm) = (++) $ "Terminal " ++ terminalToTokenSymbol tm
  showsPrec p (NonterminalSymbol s) = (++) $ "Nonterminal " ++ s

-- leaf :: Candidate -> CandidateTree
-- leaf cand = CandidateTree cand []
-- leaf cand = Leaf cand

leafs :: TokenInterface token => [CandidateTree token] -> [Candidate token]
leafs []                                     = []
-- leafs (CandidateTree leaf [] : forest)       = leaf : leafs forest
-- leafs (CandidateTree leaf subtrees : forest) =
leafs (Leaf leaf : forest) = leaf : leafs forest
leafs (Node leaf subtrees : forest) =
  leafs subtrees ++ leafs forest -- The leaf is not included as a candidate!
                                 -- i.e., leafs (Node leaf []) = []

topSymbol :: TokenInterface token => CandidateTree token -> String
topSymbol (Leaf cand) =
  case cand of
    TerminalSymbol tm -> terminalToTokenSymbol tm
    NonterminalSymbol s -> s
    
topSymbol (Node cand _) =
  case cand of
    TerminalSymbol tm -> terminalToTokenSymbol tm -- Is this possible?
    NonterminalSymbol s -> s

-----
analyzeActions :: TokenInterface token => ActionLogs token -> IO ()
analyzeActions logs =
    let map = collect logs in
        printResult map


printNoLn :: Show a => a -> IO ()
printNoLn = putStr . show

printResult :: TokenInterface token => [(State, [CandidateTree token])] -> IO ()
printResult stateCandidate =   -- [ (State, [ (CandidateForest, Integer) ] ) ]
    mapM_ pr stateCandidate
    where
        pr (state, candForest) = -- state candidate count
            do printNoLn state
               putStr " "
               putStr (concatMap prSymbolWithSp $ map topSymbol candForest)
               -- putStr " "
               -- printNoLn count
               putStrLn ""
               putStr " "
               putStr (concatMap prSymbolWithSp $ map unterminal $ leafs candForest)
               putStrLn ""            

        unterminal (TerminalSymbol (Terminal text line col maybeToken)) = 
             show line ++ "," ++ show col ++ ": " ++ text ++ "\n"
        unterminal _ = error "unlunterminal: expected TerminalSymbol (Terminal ...)"

        prSymbolWithSp sym = sym ++ " "


collect :: TokenInterface token => ActionLogs token ->[(State, [CandidateTree token])]
collect logs =
    let initState = 0
        map = onLogs initState logs []
    in  map


onLogs :: TokenInterface token => State -- ^ 
  -> [ActionLog token] -- ^ 
  -> [(State, [CandidateTree token])] -- ^ state |-> { candidate1 |-> n1, ..., candidatek |-> nk }
  -> [(State, [CandidateTree token])]
onLogs state [] map = map
onLogs state logs map =
    let candidate = initRepReduce state logs
        (maybeNextState, nextLogs) = getNextState logs

        -- foundMap = Map.fromList [ (candidate,1) ]

        -- f newMap oldMap =
        --   let (v,c) = head (Map.toList newMap) in
        --     Map.insertWith g v c oldMap
        -- g nc oc = oc + nc

        -- newMap = if null candidate then map
        --          else Map.insertWith f state foundMap map
        newMap = if null candidate then map
                 else map ++ [(state, candidate)]
    in
        -- trace ("onLogs: " ++ show state ++
        --        "        " ++ showActionLog (head logs) ++
        --        "        " ++ show candidate ++
        --        "\n") $
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


--
-- initRepReduce: shift^* Reduce 했을 때 shift^*에 의해 탐색된 심볼들을 candidate로 한다!
--
initRepReduce :: TokenInterface token => State -> [ActionLog token] -> [CandidateTree token]
initRepReduce currentState
    (LogReduce prodRuleNum prodRuleText _ : LogGoto state nonTerminal : logs)
        = [] -- initRepReduce state logs

initRepReduce currentState
    (LogReduce prodRuleNum prodRuleText _ : logs)
        = error $ "[initRepReduce] Not Goto after Reduce: "
                    ++ if null logs then "[]" else showActionLog (head logs)
                    ++ " at state " ++ show currentState

initRepReduce currentState logs = repShiftOrGoto currentState [] logs


repShiftOrGoto :: TokenInterface token => State -> [CandidateTree token] -> [ActionLog token] -> [CandidateTree token]
repShiftOrGoto currentState symTrees
    (LogShift state terminal : logs) =
        repShiftOrGoto state (symTrees ++ [Leaf (TerminalSymbol terminal)]) logs

repShiftOrGoto currentState symTrees
    (LogReduce prodRuleNum prodRuleText rhsLength : LogGoto state nonterminal : logs) =
        if length symTrees <= rhsLength then symTrees
        else repShiftOrGoto state
                (simulReduce symTrees rhsLength (NonterminalSymbol nonterminal)) logs

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
simulReduce symTrees rhsLength nonterminalSym =
    -- trace ("simulReduce: " ++ show symTrees ++
    --         "        " ++ show rhsLength ++
    --         "        " ++ show leafWithNonterminal ++
    --         "\n") $
    let children = reverse $ take rhsLength $ reverse symTrees
        leafWithNonterminal = Node nonterminalSym children
        theRestOfSymTrees = reverse $ drop rhsLength $ reverse symTrees
    in theRestOfSymTrees ++ [leafWithNonterminal]