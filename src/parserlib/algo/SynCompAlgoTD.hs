module SynCompAlgoTD (compCandidates) where

import AutomatonType
import Terminal
import TokenInterface
import CommonParserUtil
import SynCompAlgoUtil
import Config

import Data.Typeable
import Data.List (nub)

-- | Computing candidates

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
  -- compGammasDfs ccOption level symbols state stk []
  do let symbolTrees = map candidateLeaf symbols
     (candForestList,bool) <- extendedCompCandidates ccOption symbolTrees state stk
     return (map leafs candForestList, bool)

--
type State           = Int
type LengthOfSymbols = Int
  
--
extendedCompCandidates
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [CandidateTree] -> State -> Stack token ast -> IO ([[CandidateTree]], Bool)
extendedCompCandidates ccOption symbols state stk = do
  -- check yapb.config
  maybeConfig <- readConfig
  case maybeConfig of
    Nothing ->
      do list <- extendedCompCandidates' ccOption symbols state stk
         return (map (\(a,b,c)->c) list, True)
         
    Just config ->
      let r_level  = config_R_LEVEL config
          gs_level = config_GS_LEVEL config
          debugFlag = config_DEBUG config
          display  = config_DISPLAY config
          isSimple = config_SIMPLE config

          ccOption' = ccOption { cc_debugFlag = debugFlag
                               , cc_r_level = r_level
                               , cc_gs_level = gs_level
                               , cc_simpleOrNested = isSimple
                               , cc_searchState = initSearchState r_level gs_level
                               }
                      
      in
      do list <- extendedCompCandidates' ccOption' symbols state stk
         return (map (\(a,b,c)->c) list, display)

  where
    -- main function
    extendedCompCandidates' ccOption symbols state stk =
      let
         debugFlag = cc_debugFlag ccOption
         isSimple  = cc_simpleOrNested ccOption
         r_level   = cc_r_level ccOption
         gs_level  = cc_gs_level ccOption
      in
         debug debugFlag ("simple/nested(True/False): " ++ show isSimple) $ 
         debug debugFlag ("(Max) r level: " ++ show r_level) $ 
         debug debugFlag ("(Max) gs level: " ++ show gs_level) $ 
         debug debugFlag "" $ 

         -- do list <- if isSimple
         --              then do repReduce ccOption symbols state stk
         --              else do extendedNestedCandidates ccOption [(state, stk, symbols)]

         --    return [ c | (state, stk, c) <- list, null c == False ]
            
         do if isSimple
              then do repReduce ccOption symbols state stk
              else do extendedNestedCandidates ccOption [(state, stk, symbols)]


-- Extended simple candidates
-- extendedSimpleCandidates
--     :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
--        CompCandidates token ast -> State -> Stack token ast -> IO [(State, Stack token ast, [CandidateTree])]
       
-- extendedSimpleCandidates ccOption state stk = repReduce ccOption [] state stk 


-- Extended nested candidates
extendedNestedCandidates
    :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
       CompCandidates token ast -> [(State, Stack token ast, [CandidateTree])]
       -> IO [(State, Stack token ast, [CandidateTree])]
       
extendedNestedCandidates ccOption initStateStkCandsList =
  let debugFlag = cc_debugFlag ccOption
      r_level   = cc_r_level ccOption

      level     = cc_printLevel ccOption

      len       = length initStateStkCandsList

      f ((state, stk, symbols),i) =
          debug debugFlag (prlevel level ++ "[extendedNestedCandidates] : " ++ show i ++ "/" ++ show len) $ 
          debug debugFlag (prlevel level ++ " - state " ++ show state) $ 
          debug debugFlag (prlevel level ++ " - stack " ++ prStack stk) $ 
          debug debugFlag (prlevel level ++ " - symbs " ++ show symbols) $ 
          debug debugFlag "" $ 

          do list <- repReduce ccOption{cc_simpleOrNested=True,cc_printLevel=level+1} [] state stk
             return [ (state,stk,symbols++cands) | (state,stk,cands) <- list]
  in
  if r_level > 0
  then
    do stateStkCandsListList <- mapM f (zip initStateStkCandsList [1..])

       -- if null stateStkCandsListList
       --   then return initStateStkCandsList
       --   else do nextStateStkCandsList <-
       --             extendedNestedCandidates ccOption{cc_r_level=r_level-1}
       --                [ (toState, toStk, {- fromCand ++ -} toCand)

       --                | ((fromState, fromStk, fromCand), toList)
       --                    <- zip initStateStkCandsList stateStkCandsListList

       --                , (toState, toStk, toCand) <- toList
       --                ]

       --           return $ {- initStateStkCandsList ++ -} nextStateStkCandsList
                 
       extendedNestedCandidates ccOption{cc_r_level=r_level-1} (concat stateStkCandsListList)

  else
    return initStateStkCandsList  -- cf. []

repReduce
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [CandidateTree] -> State -> Stack token ast
     -> IO [(State, Stack token ast, [CandidateTree])]

repReduce ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      searchState     = cc_searchState ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in debug flag (prlevel level ++ "[repReduce] " ++ show (cc_searchState ccOption)) $

     if null [True | ((s,lookahead),Accept) <- actionTable, state==s] == False
     then 
          debug flag (prlevel level ++ "accept: " ++ show state) $ 
          return []

     else do
            case nub [prnum | ((s,lookahead),Reduce prnum) <- actionTable
                             , state==s
                             , isReducible productionRules prnum stk] of
               []        -> if isFinalReduce (cc_searchState ccOption)
                            then debug flag (prlevel level ++ "no Reduce (final search state): final " ++ show state) $
                                return []

                            else
                                repGotoOrShift False -- do not do repReduce in the beginning!
                                   (setGotoOrShift (ccOption{cc_printLevel=level+1}))
                                   -- (ccOption {cc_searchState =
                                   --            SS_GotoOrShift
                                   --              (r_level (cc_searchState ccOption))
                                   --              (gs_level (cc_searchState ccOption)) })
                                     symbols state stk

               prnumList -> do let len = length prnumList

                               listOfList <-
                                 mapM (\ (prnum, i) ->
                                         do let searchState = cc_searchState ccOption
                                            
                                            -- SS_InitReduces
                                            if isInitReduces searchState then
                                              do list2 <- repGotoOrShift True -- do repReduce since it is Init search state
                                                           (ccOption{cc_printLevel=level+1})
                                                           -- (ccOption {cc_searchState =
                                                           --              SS_GotoOrShift
                                                           --                (r_level (cc_searchState ccOption))
                                                           --                (gs_level (cc_searchState ccOption)) })
                                                             symbols state stk

                                                 list1 <- simulReduce ccOption symbols prnum len i state stk
                                                 return $ list2 ++ list1

                                            -- SS_FinalReduce
                                            else if isFinalReduce searchState then
                                              do simulReduce ccOption symbols prnum len i state stk

                                            -- SS_GotoOrShift: never reach here!
                                            else
                                              do error $ "repReduce: Unexpected search state: " ++ show searchState)

                                   (zip prnumList [1..])

                               -- list2 <- if isFinalReduce (cc_searchState ccOption)
                               --          then do return []

                               --          else repGotoOrShift
                               --                 (ccOption {cc_searchState =
                               --                              SS_GotoOrShift
                               --                                (r_level (cc_searchState ccOption))
                               --                                (gs_level (cc_searchState ccOption)) })
                               --                   symbols state stk

                               return $ concat listOfList

{- Called when Init or Final search states -}
simulReduce :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [CandidateTree]
 -> Int  -- Production rule number
 -> Int  -- of all reducible actions
 -> Int  -- ith action chosen
 -> State
 -> Stack token ast
 -> IO [(State, Stack token ast, [CandidateTree])]
simulReduce ccOption symbols prnum len i state stk =
  let flag      = cc_debugFlag ccOption
      isSimple  = cc_simpleOrNested ccOption
      automaton = cc_automaton ccOption
      searchState     = cc_searchState ccOption
      level = cc_printLevel ccOption

      productionRules = prodRules automaton
      prodrule  = (prodRules automaton) !! prnum
      lhs       = fst prodrule
      rhs       = snd prodrule
      
      rhsLength = length rhs
  in
     debug flag (prlevel level ++ "[simulReduce] " ++ show (cc_searchState ccOption)) $

     debug flag (prlevel level ++ "REDUCE [" ++ show i ++ "/" ++ show len ++ "] " ++
                 "[" ++ show (cc_searchState ccOption) ++ "] " ++
                 "at " ++ show state  ++ " " ++
                 showProductionRule (productionRules !! prnum)) $ 
     -- debug flag (prlevel level ++ " - prod rule: " ++ show (productionRules !! prnum)) $ 
     -- debug flag (prlevel level ++ " - State " ++ show state) $ 
     debug flag (prlevel level ++ " - Stack " ++ prStack stk) $ 
     debug flag (prlevel level ++ " - Symbols: " ++ show symbols) $ 
     -- debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
     debug flag "" $ 

     if rhsLength > length symbols && length symbols > 0   -- This is the time to stop!
     then
       if isFinalReduce searchState
       then
         debug flag (prlevel level ++ "rhsLength > length symbols: final") $
         debug flag "" $
         
         do let stk1     = drop (rhsLength*2) stk
            -- let children = toChildren $ reverse $ take (rhsLength*2) stk
            -- [CandidateTree (NonterminalSymbol lhs) children]
            let topState = currentState stk1
            let toState  = case lookupGotoTable (gotoTbl automaton) topState lhs of
                  Just state -> state
                  Nothing -> error $ "[simulReduce] Must not happen: lhs: "
                                     ++ lhs ++ " state: " ++ show topState
            let stk2 = push (StkNonterminal Nothing lhs) stk1  -- ast
            let stk3 = push (StkState toState) stk2
            return [(toState, stk3, symbols)]  -- Note: toState and stk3 are after the reduction, but symbols are not!! 
            
       else
        repGotoOrShift False -- Todo: do not do repReduce because Reduce has just been done???
          (ccOption{cc_printLevel=level+1})
          -- (ccOption{cc_searchState =
          --           SS_GotoOrShift
          --           (r_level (cc_searchState ccOption))
          --           (gs_level (cc_searchState ccOption)) })
            symbols state stk

     -- rhsLength <= length symbols || length symbols == 0
     else do let stk1 = drop (rhsLength*2) stk
             let topState = currentState stk1
             let toState = case lookupGotoTable (gotoTbl automaton) topState lhs of
                   Just state -> state
                   Nothing -> error $ "[simulReduce] Must not happen: lhs: "
                                      ++ lhs ++ " state: " ++ show topState
             let stk2 = push (StkNonterminal Nothing lhs) stk1  -- ast
             let stk3 = push (StkState toState) stk2

             let (reducedSymbols, gs) =
                   if rhsLength <= length symbols
                   then let revSymbols = reverse symbols
                            children   = reverse (take rhsLength revSymbols)
                            therest    = drop rhsLength $ revSymbols
                        in  ( reverse $ (candidateNode (NonterminalSymbol lhs) children :) $ therest
                            , cc_gs_level ccOption + rhsLength - 1)
                   else (symbols, cc_gs_level ccOption)

             if isSimple then  -- simple mode

               if isInitReduces searchState then -- reduces until symbols are found
                 do -- listOfList <- repReduce ccOption{cc_printLevel=level+1} reducedSymbols toState stk3
                    repReduce ccOption{cc_printLevel=level+1,cc_gs_level=gs} reducedSymbols toState stk3

                    -- let f syms0 (s, stk, syms) = (s, stk, syms0 ++ syms)

                    -- return (if null symbols
                    --         then listOfList
                    --         else {- (toState, stk3, symbols) : -} map (f symbols) listOfList)  -- Q: symbols: 필요?
                    
               else if isFinalReduce searchState then  -- Todo: isFinalReduce???
                 -- Todo(important): What would happen if it just returns []???
                 do repReduce ccOption{cc_printLevel=level+1} reducedSymbols toState stk3 -- Just copied the code above!
                    
                 -- do return (if null symbols
                 --            then []
                 --            else [(toState, stk3, symbols)])
                    
               else -- SS_GotoOrShift
                 do error $ "simulReduce: Unexpected search state" ++ show searchState

             else -- nested mode
               do error $ "simulReduce: Unexpected nested mode: "


simulGoto :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [CandidateTree]
 -> Int
 -> [StkElem token ast]
 -> IO [(State, Stack token ast, [CandidateTree])]
simulGoto ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in do debug flag (prlevel level ++ "[simulGoto] " ++ show (cc_searchState ccOption)) $

          case nub [ (nonterminal,toState)
                   | ((fromState,nonterminal),toState) <- gotoTable
                   , state==fromState ] of
            [] -> do return []

            nontermStateList ->
              do
                let len = length nontermStateList
                listOfList <-
                  mapM (\ ((nonterminal,snext),i) -> 
                            let stk1 = push (StkNonterminal Nothing nonterminal) stk in
                            let stk2 = push (StkState snext) stk1 in

                            debug flag (prlevel level ++ "GOTO [" ++ show i ++ "/" ++ show len ++ "] " ++
                                           "[" ++ show (cc_searchState ccOption) ++ "] " ++
                                           "at " ++ show state ++ " -> " ++ show nonterminal ++ " -> " ++ show snext) $ 
                            debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                            debug flag (prlevel level ++ " - " ++ "Symbols:" ++ show (symbols++[candidateNode (NonterminalSymbol nonterminal) []])) $ 
                            -- debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
                            debug flag "" $ 

                            repGotoOrShift True
                              (setGotoOrShift (ccOption{cc_printLevel=level+1}))
                                (symbols++[candidateNode (NonterminalSymbol nonterminal) []])
                                  snext stk2)
                    (zip nontermStateList [1..])

                return $ concat listOfList

simulShift :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [CandidateTree]
 -> Int
 -> [StkElem token ast]
 -> IO [(State, Stack token ast, [CandidateTree])]
simulShift ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in
  let cand2 = nub [(terminal,snext) | ((s,terminal),Shift snext) <- actionTable, state==s]
      len = length cand2
  in do debug flag (prlevel level ++ "[simulShift] " ++ show (cc_searchState ccOption)) $

          case cand2 of
           [] -> do return []

           _  -> do
                    listOfList <-
                      mapM (\ ((terminal,snext),i)-> 
                                let stk1 = push (StkTerminal (Terminal terminal 0 0 Nothing)) stk in
                                let stk2 = push (StkState snext) stk1 in

                                debug flag (prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "] " ++
                                              "[" ++ show (cc_searchState ccOption) ++ "] " ++
                                              "at " ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext) $ 
                                debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                                debug flag (prlevel level ++ " - " ++ "Symbols: " ++ show (symbols++[candidateNode (TerminalSymbol terminal) []])) $ 
                                -- debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
                                debug flag "" $ 

                                repGotoOrShift True
                                  (setGotoOrShift (ccOption{cc_printLevel=level+1}))
                                    (symbols++[candidateNode (TerminalSymbol terminal) []])
                                      snext stk2)
                        (zip cand2 [1..])

                    return $ concat listOfList

{- Search states are InitReduce or GotoOrShift! -}
repGotoOrShift
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     Bool ->  -- True : do repReuce, False : skip repReduce!
     CompCandidates token ast -> [CandidateTree] -> State -> Stack token ast -> IO [(State, Stack token ast, [CandidateTree])]

repGotoOrShift doRepReduce ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in
     debug flag (prlevel level ++ "[repGotoOrShift] " ++ show doRepReduce ++ ":" ++ show (cc_searchState ccOption)) $

     do if null [True | ((s,lookahead),Accept) <- actionTable, state==s] == False
        then 
             debug flag (prlevel level ++ "accept: " ++ show state) $ 
             return []

        else do
                listOfList1 <-
                  if doRepReduce
                  then repReduce
                           (ccOption{cc_printLevel = level+1,
                                     cc_searchState=
                                       SS_FinalReduce
                                         (r_level (cc_searchState ccOption))
                                         (gs_level (cc_searchState ccOption))})
                             symbols state stk
                  else return []

                -- Idea: Once this is called, do Shift or Goto at least once whenever possible
                --       by having the disjunct, isInitReduces (cc_searchState ccOption)!!

                --    listOfList1 == [] && init reduce    ==> do goto or shift
                -- || listOfList1 == [] && goto or shift  ==> do goto or shift
                -- || listOfList1 /= [] && init reduce    ==> do goto of shift (give a chance!)
                -- || listOfList1 /= [] && goto or shift  ==> No!!

                if    null listOfList1 == False && isInitReduces (cc_searchState ccOption)
                   || null listOfList1 == True
                  then
                       -- let listOfList1 = []

                       if gs_level (cc_searchState ccOption) - 1 > 0 then
                         let ccOption' = ccOption{cc_searchState=
                                             SS_GotoOrShift
                                               (r_level (cc_searchState ccOption))
                                               (gs_level (cc_searchState ccOption) - 1)}
                                               -- Decrease the gs level by one!!
                         in

                         -- both goto and shift only once 
                         if gs_level (cc_searchState ccOption) == cc_gs_level ccOption
                         then

                           do listOfList2 <- simulGoto ccOption' symbols state stk
                              listOfList3 <- simulShift ccOption' symbols state stk

                              debug flag (prlevel level ++ "[repGotoOrShift] (1) final") $
                               debug flag "" $
                                if null (listOfList1 ++ listOfList2 ++ listOfList3)
                                then return []
                                else return (listOfList1 ++ listOfList2 ++ listOfList3)

                         else

                           do listOfList2 <- simulGoto ccOption' symbols state stk

                              if null listOfList2
                              then

                                do listOfList3 <- simulShift ccOption' symbols state stk

                                   debug flag (prlevel level ++ "[repGotoOrShift] (2) final") $
                                    debug flag "" $
                                     if null (listOfList1 ++ listOfList2 ++ listOfList3)
                                     then return []
                                     else return (listOfList1 ++ listOfList2 ++ listOfList3)

                              else
                                debug flag (prlevel level ++ "[repGotoOrShift] (3): final") $
                                 debug flag "" $
                                  if null (listOfList1 ++ listOfList2)
                                  then return []
                                  else return (listOfList1 ++ listOfList2)

                       else
                         debug flag (prlevel level ++ "gs_level == 0: final") $
                         debug flag (prlevel level) $
                         do return []

                  else
                    debug flag (prlevel level ++ "[repGotoOrShift] (4) final") $
                     debug flag "" $
                      do return listOfList1

-- Todo: repReduce를 하지 않고
--       Reduce 액션이 있는지 보고
--       없으면 goto or shift 진행하고
--       있으면 reduce한번하고 종료!

--       현재 구현은 repReduce 결과가 널인지 검사해서 진행 또는 종료
--       Reduce 액션이 있어도 진행될 수 있음!
