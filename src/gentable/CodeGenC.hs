module CodeGenC where

import Data.List(groupBy)

import CFG
import ParserTable 
import GenLRParserTable
import SampleGrammar

--------------------------------------------------------------------------------
-- C Code Generation for Parser
--------------------------------------------------------------------------------

-- cgStates iss
-- cgNonterminals augCfg
-- cgGotoTable augCfg

-- C enum type declaration for states
cgStates iss = cgEnum "STATE" (cgStates' iss)
     
cgStates' [] = return ()  
cgStates' [is] = 
  do putStr "\t"
     cgState is

cgStates' [is1,is2] = 
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStrLn ""

cgStates' [is1,is2,is3] = 
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStr ", "
     cgState is3
     putStrLn ""

cgStates' [is1,is2,is3,is4] = 
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStr ", "
     cgState is3
     putStr ", "
     cgState is4
     putStrLn ""
     
cgStates' [is1,is2,is3,is4,is5] = 
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStr ", "
     cgState is3
     putStr ", "
     cgState is4
     putStr ", "
     cgState is5
     putStrLn ""
     
cgStates' (is1:is2:is3:is4:is5:iss) =
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStr ", "
     cgState is3
     putStr ", "
     cgState is4
     putStr ", "
     cgState is5
     putStrLn ","
     cgStates' iss
     
cgState is = putStr (cgToState is) 
     
cgToState is = "S" ++ cgToState' is

cgToState' []     = ""
cgToState' [i]    = show i
cgToState' (i:is) = show i ++  "_" ++ cgToState' is

-- C enum type declaration for nonterminals

cgNonterminals augCfg = 
  cgEnum "Nonterminal" (cgNonterminals' (cgCNames (nonterminals augCfg)))
    
cgNonterminals' []     = return ()    
cgNonterminals' [x]    = 
  do putStr "\t"
     putStr x
     putStrLn ""
cgNonterminals' [x1,x2]    = 
  do putStr "\t"
     putStr x1
     putStr ", "
     putStr x2
     putStrLn ""
cgNonterminals' (x1:x2:xs) = 
  do putStr "\t"
     putStr x1
     putStr ", "
     putStr x2
     putStr ", "
     putStrLn ""
     cgNonterminals' xs
     
cgCNames nts = map cgToCName nts

cgToCName x = "NONTERMINAL_" ++ cgToCName' x

cgToCName' []     = []      -- CAUTION: Don't use S' with S_ for nonterminals.
cgToCName' (c:cs) = 
  (if c == '\'' then '_' else c) : cgToCName' cs 

cgEnum name action =
  do putStrLn ("enum " ++ name ++ " {")
     action
     putStrLn "};"

-- C array for goto_table
cgGotoTable augCfg =
  do prGotoTableDim (length iss) (length nts)
     prGotoTableArr iss nts gotoTbl
  where
    (_,_,iss,_,gotoTbl) = calcLALRParseTable augCfg
    nts                 = nonterminals augCfg
    
cg_noofstates   = "NOOFSTATES"
cg_noofnonterms = "NOOFNONTERMINALS"
  
prGotoTableDim no_states no_nonterms =    
  do putStrLn $ "#define " ++ cg_noofstates   ++ " " ++ show no_states
     putStrLn $ "#define " ++ cg_noofnonterms ++ " " ++ show no_nonterms
     putStrLn ""
     
prGotoTableArr :: [[Int]] -> [String] -> LALRGotoTable -> IO ()
prGotoTableArr states nonterms gotoTbl = 
  do putStrLn $ "int goto_table[" ++ cg_noofstates ++ 
       "][" ++ cg_noofnonterms ++ "] = {"
     prGotoTableArr' states nonterms gotoTbl
     putStrLn $ "};"

prGotoTableArr' [i] nonterms gotoTbl = 
  do putStr "\t"
     putStr "{"
     prGotoTableArr'' i nonterms gotoTbl
     putStrLn "}"
prGotoTableArr' (i:states) nonterms gotoTbl = 
  do putStr "\t"
     putStr "{"
     prGotoTableArr'' i nonterms gotoTbl
     putStrLn "},"
     prGotoTableArr' states nonterms gotoTbl
     
prGotoTableArr'' i [x] gotoTbl =
  case lookupTable i (Nonterminal x) gotoTbl of
    Nothing -> do putStr $ show (-1)
    Just k  -> do putStr $ cgToState k
prGotoTableArr'' i (x:nonterms) gotoTbl =
  case lookupTable i (Nonterminal x) gotoTbl of
    Nothing -> do putStr $ show (-1) ++ ","
                  prGotoTableArr'' i nonterms gotoTbl
    Just k  -> do putStr $ cgToState k ++ ","
                  prGotoTableArr'' i nonterms gotoTbl
                  
-- Generate C code for an LALR action table
cgActionsInStates augCfg =
  do let nTabs = 1
     prTab nTabs
     putStrLn "switch( top() )"
     prTab nTabs
     putStrLn "{"
     mapM_ (\t -> cgInStates nTabs t iprules) (groupBy eqState lalrActTbl)
     prTab nTabs
     putStrLn "} /* switch ( top() ) */ "
     
  where
    CFG start prules     = augCfg
    iprules              = zip [0..] prules 
    (_,_,_,lalrActTbl,_) = calcLALRParseTable augCfg
    
    eqState (x1,_,_) (x2,_,_) = x1 == x2
     
cgInStates n ((state,extSym,acts):lalrActTbl) iprules =
  do prTab n
     putStrLn $ "case " ++ cgToState state  ++ ":"
     cgActions (n+1) ((state,extSym,acts):lalrActTbl) iprules
     prTab (n+1)
     putStrLn "break;"
     putStrLn ""
cgInStates n [] iprules
  = return ()
     
cgActions n lalrActTbl iprules =
  do prTab n
     putStrLn "switch ( toks[current_tok] )"
     prTab n
     putStrLn "{"
     
     cgActions' n lalrActTbl iprules
     
     prTab n
     putStrLn "default:"
     prTab (n+1)
     putStrLn "error = REJECT;"
     prTab (n+1)
     putStrLn "break;"
     putStrLn ""
     
     prTab n
     putStrLn "}"
  
cgActions' n [] iprules = return ()
cgActions' n ((_,extsym,action):extSymActs) iprules =
  do cgAction n extsym action iprules
     cgActions' n extSymActs iprules

cgAction n extsym (LALRShift state) iprules =
  do prTab n
     cgActionCase extsym
     prTab (n+1)
     putStrLn $ "push (" ++ cgTerminalName extsym  ++ ");"
     prTab (n+1)
     putStrLn $ "push (" ++ cgToState state ++ ");"
     prTab (n+1)
     putStrLn $ "current_tok += " ++ show (offset extsym) ++ ";"
     prTab (n+1)
     putStrLn "break;"
     putStrLn ""
     
cgAction n extsym (LALRAccept) iprules =
  do prTab n 
     cgActionCase extsym
     prTab (n+1)
     putStrLn "error = ACCEPT;"
     prTab (n+1)
     putStrLn "break;"
     
cgAction n extsym (LALRReduce i) iprules =
  case maybeprule of
    Nothing -> error $ "cgActionsInState: Cannot find " ++ show i ++ " prule"
    Just (ProductionRule y ys) -> cgAction' n extsym y ys i 
  where
    maybeprule = lookup i iprules
     
cgAction n extsym (LALRReject) iprules =     
  error "cgActionsInState: LALRReject unexpected"
     
cgAction' n extsym y ys i =
  do prTab n
     cgActionCase extsym
     mapM_ (\i -> do { prTab (n+1); putStrLn "pop();" }) [1..length ys * 2]
     putStrLn ""
     prTab (n+1)
     putStrLn "next = top();"
     prTab (n+1)
     putStrLn $ "push (" ++ cgToCName y  ++ ");"
     prTab (n+1)
     putStrLn $ "next = goto_table[next][" ++ cgToCName y ++ "];"
     prTab (n+1)
     putStrLn "if (0 <= next) push (next); else error = next;"
     prTab (n+1)
     putStrLn "break;"
     
-- Attribute of tokens specific to g3
offset (Symbol (Terminal "var")) = 3
offset _                         = 1
     
cgActionCase extsym =
  putStrLn $ "case " ++ cgTerminalName extsym ++ ":"

    
cgTerminalName extsym = 
  case extsym of
    Symbol (Terminal t) -> cgTerminalName' t
    EndOfSymbol -> cgNameEndOfSymbol
    _ -> error "cgTerminalName: not a terminal symbol"
    
cgTerminalName' t =     
  case lookup t g3_attrib_terminals of
    Nothing -> error $ "cgTerminalName: not found " ++ t
    Just y  -> y
    
-- The attribute of $
cgNameEndOfSymbol = "ENDOFSYMBOL"
  
prTab 0 = return ()     
prTab n = 
  do putStr "\t"
     prTab (n-1)