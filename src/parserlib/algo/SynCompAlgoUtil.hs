module SynCompAlgoUtil where

import AutomatonType
import Debug.Trace (trace)

-- | Candidates
data Candidate = -- data Candidate vs. data EmacsDataItem = ... | Candidate String 
    TerminalSymbol String
  | NonterminalSymbol String
  deriving Eq

data CandidateTree = CandidateTree Candidate [CandidateTree] deriving (Eq,Show)

type CandidateForest = [CandidateTree]

instance Show Candidate where
  showsPrec p (TerminalSymbol s) = (++) $ "Terminal " ++ s
  showsPrec p (NonterminalSymbol s) = (++) $ "Nonterminal " ++ s

leaf :: Candidate -> CandidateTree
leaf cand = CandidateTree cand []

leafs :: CandidateForest -> [Candidate]
leafs []                                     = []
leafs (CandidateTree leaf [] : forest)       = leaf : leafs forest
leafs (CandidateTree leaf subtrees : forest) =
  leafs subtrees ++ leafs forest -- Here, the leaf is not included as a candidate!

-- | Automation information
data Automaton token ast =
  Automaton {
    actTbl    :: ActionTable,
    gotoTbl   :: GotoTable,
    prodRules :: ProdRules
  }

-- | Computing candidates
data CompCandidates token ast = CompCandidates {
    cc_debugFlag :: Bool,
    cc_printLevel :: Int,  
    cc_maxLevel :: Int,  
    
    cc_r_level :: Int,         -- for new algorithm
    cc_gs_level :: Int,        --
    
    cc_simpleOrNested :: Bool,
    cc_automaton :: Automaton token ast,
    cc_searchState :: SearchState
  }


-- | Search states used in the algorithms

type R_Level  = Int
type GS_Level = Int

data SearchState =
    SS_InitReduces R_Level GS_Level -- Reduce^*
  | SS_GotoOrShift R_Level GS_Level -- (Goto | Shift)
  | SS_FinalReduce R_Level GS_Level -- Reduce
  deriving Show

init_r_level :: R_Level
init_r_level = 1

init_gs_level :: GS_Level
init_gs_level = 5

initSearchState r gs = SS_InitReduces r gs

isInitReduces (SS_InitReduces _ _) = True
isInitReduces _                    = False

isFinalReduce (SS_FinalReduce _ _) = True
isFinalReduce _                    = False

r_level (SS_InitReduces r gs) = r
r_level (SS_GotoOrShift r gs) = r
r_level (SS_FinalReduce r gs) = r

gs_level (SS_InitReduces r gs) = gs
gs_level (SS_GotoOrShift r gs) = gs
gs_level (SS_FinalReduce r gs) = gs

-- | Utilities
debug :: Bool -> String -> a -> a
debug flag msg x = if flag then trace msg x else x

multiDbg [] = \x -> x
multiDbg (f:fs) = f . multiDbg fs

prlevel :: Int -> String
prlevel n = take n (let spaces = ' ' : spaces in spaces)

showProductionRule :: (String,[String]) -> String
showProductionRule (lhs,rhss) =
  lhs ++ " -> " ++ concat (map (\rhs -> rhs ++ " ") rhss)