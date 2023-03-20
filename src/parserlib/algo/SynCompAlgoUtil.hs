module SynCompAlgoUtil where

import AutomatonType
import Debug.Trace (trace)

-- | Candidates
data Candidate = -- data Candidate vs. data EmacsDataItem = ... | Candidate String 
    TerminalSymbol !String
  | NonterminalSymbol !String
  deriving (Eq, Ord)

-- | Candidate tree

-- data CandidateTree = CandidateTree Candidate [CandidateTree] deriving (Eq,Show)

{-
--   ( CandidateTree cand [] )^*          ===> Leaf cand
--   ( CandidateTree cand [t1,...,tn] )^* ===> Node cand [t1^*, ..., tn^*]
--
--   A ->                ===> Node (Nonterminal "A") []
--                            is not equal to Leaf (Nonterminal "A").
-}

data CandidateTree =
    Leaf !Candidate 
  | Node !Candidate ![CandidateTree]
  deriving (Eq,Show,Ord)

type CandidateForest = [CandidateTree]

instance Show Candidate where
  showsPrec p (TerminalSymbol s) = (++) $ "Terminal " ++ s
  showsPrec p (NonterminalSymbol s) = (++) $ "Nonterminal " ++ s

-- leaf :: Candidate -> CandidateTree
-- leaf cand = CandidateTree cand []
-- leaf cand = Leaf cand

leafs :: CandidateForest -> [Candidate]
leafs []                                     = []
-- leafs (CandidateTree leaf [] : forest)       = leaf : leafs forest
-- leafs (CandidateTree leaf subtrees : forest) =
leafs (Leaf leaf : forest) = leaf : leafs forest
leafs (Node leaf subtrees : forest) =
  leafs subtrees ++ leafs forest -- The leaf is not included as a candidate!
                                 -- i.e., leafs (Node leaf []) = []

topSymbol :: CandidateTree -> String
topSymbol (Leaf cand) =
  case cand of
    TerminalSymbol s -> s
    NonterminalSymbol s -> s
    
topSymbol (Node cand _) =
  case cand of
    TerminalSymbol s -> s
    NonterminalSymbol s -> s

--
candidateLeaf :: Candidate -> CandidateTree
candidateLeaf cand = Leaf cand

candidateNode :: Candidate -> CandidateForest -> CandidateTree
candidateNode nonterminal_lhs children = Node nonterminal_lhs children

-- | Automation information
data Automaton token ast =
  Automaton {
    actTbl    :: ActionTable,
    gotoTbl   :: GotoTable,
    prodRules :: ProdRules
  }

-- | Computing candidates
data CompCandidates token ast = CompCandidates {
    cc_debugFlag :: !Bool,
    cc_printLevel :: !Int,  
    cc_maxLevel :: !Int,  
    
    cc_r_level :: !Int,         -- for new algorithm
    cc_gs_level :: !Int,        --
    
    cc_simpleOrNested :: !Bool,
    cc_automaton :: !(Automaton token ast),
    cc_searchState :: !SearchState,

    cc_isAbleToSearch :: String -> Bool 
  }


-- | Search states used in the algorithms

type R_Level  = Int
type GS_Level = Int

data SearchState =
    SS_InitReduces !R_Level !GS_Level -- Reduce^*
  | SS_GotoOrShift !R_Level !GS_Level -- (Goto | Shift)
  | SS_FinalReduce !R_Level !GS_Level -- Reduce

instance Show SearchState where
  showsPrec p (SS_InitReduces r gs) = (++) $ "I:" ++ show r ++ ":" ++ show gs
  showsPrec p (SS_GotoOrShift r gs) = (++) $ "M:" ++ show r ++ ":" ++ show gs
  showsPrec p (SS_FinalReduce r gs) = (++) $ "F:" ++ show r ++ ":" ++ show gs

init_r_level :: R_Level
init_r_level = 1

init_gs_level :: GS_Level
init_gs_level = 5

initSearchState r gs = SS_InitReduces r gs

isInitReduces (SS_InitReduces _ _) = True
isInitReduces _                    = False

isFinalReduce (SS_FinalReduce _ _) = True
isFinalReduce _                    = False

setGotoOrShift ccOption =
  case cc_searchState ccOption of
    SS_InitReduces r gs -> ccOption{cc_searchState=SS_GotoOrShift r gs}
    SS_GotoOrShift r gs -> ccOption{cc_searchState=SS_GotoOrShift r gs}
    SS_FinalReduce r gs -> ccOption{cc_searchState=SS_GotoOrShift r gs} -- Todo: ??? error $ "[setGotoOrShift] expected SS_InitReduces or SS_GotoOrShift"
    
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
