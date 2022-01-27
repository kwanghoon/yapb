module SynCompAlgoPEPM (compCandidates) where

import AutomatonType
import Config
import Terminal
import TokenInterface
import CommonParserUtil
import SynCompAlgoUtil

import Data.Typeable
import Data.List (nub)

-- |
compCandidates
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast
     -> Int
     -> [Candidate]
     -> Int
     -> Stack token ast
     -> IO ([[Candidate]], Bool)

compCandidates ccOption level symbols state stk = 
  let flag = cc_debugFlag ccOption in
  debug flag "" $ 
  debug flag "[compCandidates] " $ 
  debug flag (" - state: " ++ show state) $
  debug flag (" - stack: " ++ prStack stk) $ 
  debug flag "" $ 
    do cands <- compGammasDfs ccOption level symbols state stk []
       -- do extendedCompCandidates ccOption symbols state stk
       maybeConfig <- readConfig
       case maybeConfig of
         Nothing     -> return (cands, True) -- Display is set to True???
         Just config -> return (cands, config_DISPLAY config)

-- | 
compGammasDfs
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast
     -> Int
     -> [Candidate]
     -> Int
     -> Stack token ast
     -> [(Int, Stack token ast, String)]
     -> IO [[Candidate]]

compGammasDfs ccOption level symbols state stk history =
  let flag = cc_debugFlag ccOption
      maxLevel = cc_maxLevel ccOption
      printLevel = cc_printLevel ccOption
      isSimple = cc_simpleOrNested ccOption
      automaton = cc_automaton ccOption
      
      actionTable = actTbl automaton
      gotoTable = gotoTbl automaton
      productionRules = prodRules automaton
  in
  if level > maxLevel then 
    let result_symbols = if null symbols then [] else [symbols] in
    debug flag (prlevel level ++ "maxlevel reached.") $ 
    debug flag (prlevel level ++ " - " ++ show result_symbols) $ 
    do return result_symbols
  else
    checkCycle flag False level state stk "" history
     (\history ->
       {- 1. Reduce -}
       case nub [prnum | ((s,lookahead),Reduce prnum) <- actionTable, state==s, isReducible productionRules prnum stk] of
         [] ->
           {- 2. Goto table -}
           debug flag (prlevel level ++ "no reduce: " ++ show state) $ 
           case nub [(nonterminal,toState) | ((fromState,nonterminal),toState) <- gotoTable, state==fromState] of
             [] -> 
               debug flag (prlevel level ++ "no goto: " ++ show state) $
               {- 3. Accept -}
               if length [True | ((s,lookahead),Accept) <- actionTable, state==s] >= 1
               then 
                    debug flag (prlevel level ++ "accept: " ++ show state) $
                    return []
               {- 4. Shift -}
               else let cand2 = nub [(terminal,snext) | ((s,terminal),Shift snext) <- actionTable, state==s] in
                    let len = length cand2 in
                    case cand2 of
                     [] -> 
                       debug flag (prlevel level ++ "no shift: " ++ show state) $ 
                       return []

                     _  -> do listOfList <-
                                mapM (\ ((terminal,snext),i)-> 
                                   let stk1 = push (StkTerminal (Terminal terminal 0 0 Nothing)) stk in -- Todo: ??? (toToken terminal)
                                   let stk2 = push (StkState snext) stk1 in

                                   debug flag (prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "]: "
                                                ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext) $ 
                                   debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                                   debug flag (prlevel level ++ " - " ++ "Symbols: " ++ show (symbols++[TerminalSymbol terminal])) $ 
                                   debug flag "" $ 

                                   checkCycle flag True level snext stk2 terminal history
                                     (\history1 -> 
                                      compGammasDfs ccOption (level+1) (symbols++[TerminalSymbol terminal]) snext stk2 history1) )
                                        (zip cand2 [1..])
                              return $ concat listOfList
             nontermStateList -> do
               let len = length nontermStateList

               listOfList <-
                 mapM (\ ((nonterminal,snext),i) ->
                    let stk1 = push (StkNonterminal Nothing nonterminal) stk
                        stk2 = push (StkState snext) stk1
                    in 
                    -- checkCycle False level snext stk2 ("GOTO " ++ show snext ++ " " ++ nonterminal) history
                    -- checkCycle True level state stk nonterminal history
                    checkCycle flag True level snext stk2 nonterminal history

                      (\history1 -> 
                       debug flag (prlevel level ++ "GOTO [" ++ show i ++ "/" ++ show len ++ "] at "
                                     ++ show state ++ " -> " ++ show nonterminal ++ " -> " ++ show snext) $ 
                       debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                       debug flag (prlevel level ++ " - " ++ "Symbols:" ++ show (symbols++[NonterminalSymbol nonterminal])) $ 
                       debug flag "" $ 

                       compGammasDfs ccOption (level+1) (symbols++[NonterminalSymbol nonterminal]) snext stk2 history1) )
                         (zip nontermStateList [1..])
               return $ concat listOfList

         prnumList ->
           let len = length prnumList in

           debug flag (prlevel level     ++ "# of prNumList to reduce: " ++ show len ++ " at State " ++ show state) $ 
           debug flag (prlevel (level+1) ++ show [ productionRules !! prnum | prnum <- prnumList ]) $ 

           do listOfList <-
               mapM (\ (prnum,i) -> (
                 -- checkCycle False level state stk ("REDUCE " ++ show prnum) history
                 checkCycle flag True level state stk (show prnum) history
                   (\history1 -> 
                      debug flag (prlevel level ++ "REDUCE"
                                      ++ " [" ++ show i ++ "/" ++ show len ++ "]") $ 
                      debug flag (prlevel level ++ " - prod rule: " ++ show (productionRules !! prnum)) $ 
                      debug flag (prlevel level ++ " - State " ++ show state) $ 
                      debug flag (prlevel level ++ " - Stack " ++ prStack stk) $ 
                      debug flag (prlevel level ++ " - Symbols: " ++ show symbols) $ 
                      debug flag "" $ 
                      compGammasDfsForReduce ccOption level symbols state stk history1 prnum)) )
                    (zip prnumList [1..])
              return $ concat listOfList )
  
compGammasDfsForReduce ccOption level symbols state stk history prnum = 
  let flag = cc_debugFlag ccOption
      isSimple = cc_simpleOrNested ccOption
      automaton = cc_automaton ccOption

      prodrule   = (prodRules automaton) !! prnum
      lhs = fst prodrule
      rhs = snd prodrule
      
      rhsLength = length rhs
  in 
  if ( {- rhsLength == 0 || -} (rhsLength > length symbols) ) == False
  then
    debug flag (prlevel level ++ "[LEN COND: False] length rhs > length symbols: NOT "
                   ++ show rhsLength ++ ">" ++ show (length symbols)) $ 
    debug flag ("rhs: " ++ show rhs) $ 
    debug flag ("symbols: " ++ show symbols) $ 
    return [] -- Todo: (if null symbols then [] else [symbols])
    
  else    -- reducible
    let stk1 = drop (rhsLength*2) stk in
    let topState = currentState stk1 in
    let toState =
         case lookupGotoTable (gotoTbl automaton) topState lhs of
           Just state -> state
           Nothing -> error $ "[compGammasDfsForReduce] Must not happen: lhs: "
                                ++ lhs ++ " state: " ++ show topState
    in                            
    let stk2 = push (StkNonterminal Nothing lhs) stk1 in -- ast
    let stk3 = push (StkState toState) stk2 in
    debug flag (prlevel level ++ " - GOTO : "
                   ++ show topState ++ " " ++ lhs ++ " " ++ show toState) $ 
    debug flag (prlevel level ++ " --- Stack " ++ prStack stk3) $ 
    debug flag "" $ 

    debug flag (prlevel level ++ "Found a gamma: " ++ show symbols) $ 
    debug flag "" $ 

    if isSimple  && not (null symbols) -- Todo: loosley simple mode:  +  && not (null symbols)
    then return (if null symbols then [] else [symbols])
    else do listOfList <- compGammasDfs ccOption (level+1) [] toState stk3 history
            return (if null symbols then listOfList else (symbols : map (symbols ++) listOfList))
            
