module SynCompAlgoBU (compCandidates) where

import AutomatonType
import Terminal
import TokenInterface
import CommonParserUtil
import SynCompAlgoUtil
import Config

import Data.Typeable
import Data.List (nub)

--
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
  do extendedCompCandidates ccOption symbols state stk


--------------------------------------------------------------------------------
-- A new search algorithm
--------------------------------------------------------------------------------
--
type State           = Int
type LengthOfSymbols = Int
  
--
extendedCompCandidates
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [Candidate] -> State -> Stack token ast -> IO ([[Candidate]], Bool)
extendedCompCandidates ccOption symbols state stk = do
  maybeConfig <- readConfig
  case maybeConfig of
    Nothing ->
      do list <- extendedCompCandidates' ccOption symbols state stk
         return (list, True)
         
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
         return (list, display)

  where
    extendedCompCandidates' ccOption symbols state stk =
      let
         debugFlag = cc_debugFlag ccOption
         isSimple  = cc_simpleOrNested ccOption
         r_level   = cc_r_level ccOption
         gs_level  = cc_gs_level ccOption
      in
         debug debugFlag ("simple(True)/nested(False): " ++ show isSimple) $ 
         debug debugFlag ("(Max) r level: " ++ show r_level) $ 
         debug debugFlag ("(Max) gs level: " ++ show gs_level) $ 
         debug debugFlag "" $ 

         do list <- if isSimple
                      then repReduce ccOption symbols state stk 
                      else do extendedNestedCandidates ccOption [(state, stk, symbols)]

            return [ c | (state, stk, c) <- list, null c == False ]


-- Extended simple candidates
extendedSimpleCandidates
    :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
       CompCandidates token ast -> State -> Stack token ast -> IO [(State, Stack token ast, [Candidate])]
       
extendedSimpleCandidates ccOption state stk = repReduce ccOption [] state stk 


-- Extended nested candidates
extendedNestedCandidates
    :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
       CompCandidates token ast -> [(State, Stack token ast, [Candidate])]
       -> IO [(State, Stack token ast, [Candidate])]
       
extendedNestedCandidates ccOption initStateStkCandsList =
  let f (state, stk, symbols) =
          debug debugFlag "Given " $ 
          debug debugFlag (" - state " ++ show state) $ 
          debug debugFlag (" - stack " ++ prStack stk) $ 
          debug debugFlag (" - cand  " ++ show symbols) $ 
          debug debugFlag "" $ 

          do repReduce ccOption{cc_simpleOrNested=True} {- symbols -} [] state stk

      debugFlag = cc_debugFlag ccOption
      r_level   = cc_r_level ccOption
  in
  if r_level > 0
  then
    debug debugFlag "[extendedNestedCandidates] :" $ 
      multiDbg (map (\(state, stk, cand) ->
                     debug debugFlag (" - state " ++ show state) $
                     debug debugFlag (" - stack " ++ prStack stk) $
                     debug debugFlag (" - cand  " ++ show cand) $
                     debug debugFlag ("")
                ) initStateStkCandsList) $

    do stateStkCandsListList <- mapM f initStateStkCandsList

       if null stateStkCandsListList
         then return initStateStkCandsList
         else do nextStateStkCandsList <-
                   extendedNestedCandidates ccOption{cc_r_level=r_level-1}
                      [ (toState, toStk, fromCand ++ toCand)

                      | ((fromState, fromStk, fromCand), toList)
                          <- zip initStateStkCandsList stateStkCandsListList

                      , (toState, toStk, toCand) <- toList
                      ]

                 return $ initStateStkCandsList ++ nextStateStkCandsList

  else
    return []

repReduce
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [Candidate] -> State -> Stack token ast
     -> IO [(State, Stack token ast, [Candidate])]

repReduce ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      searchState     = cc_searchState ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in -- do debug flag $ prlevel level ++ "[repReduce] " ++ show (cc_searchState ccOption)

     if null [True | ((s,lookahead),Accept) <- actionTable, state==s] == False
     then 
          debug flag (prlevel level ++ "accept: " ++ show state) $ 
          return []

     else do
            case nub [prnum | ((s,lookahead),Reduce prnum) <- actionTable
                             , state==s
                             , isReducible productionRules prnum stk] of
               []        -> if isFinalReduce (cc_searchState ccOption)
                            then return []

                            else repGotoOrShift
                                   (ccOption {cc_searchState =
                                              SS_GotoOrShift
                                                (r_level (cc_searchState ccOption))
                                                (gs_level (cc_searchState ccOption)) })
                                     symbols state stk

               prnumList -> do let len = length prnumList

                               listOfList <-
                                 mapM (\ (prnum, i) ->
                                         do let searchState = cc_searchState ccOption
                                            
                                            -- SS_InitReduces
                                            if isInitReduces searchState then
                                              do list2 <- repGotoOrShift
                                                           (ccOption {cc_searchState =
                                                                        SS_GotoOrShift
                                                                          (r_level (cc_searchState ccOption))
                                                                          (gs_level (cc_searchState ccOption)) })
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

simulReduce :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [Candidate]
 -> Int  -- Production rule number
 -> Int  -- of all reducible actions
 -> Int  -- ith action chosen
 -> State
 -> Stack token ast
 -> IO [(State, Stack token ast, [Candidate])]
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
     -- debug flag $ prlevel level ++ "[simulReduce] " ++ show (cc_searchState ccOption)

     debug flag (prlevel level ++ "REDUCE [" ++ show i ++ "/" ++ show len ++ "] at ") $ 
     debug flag (prlevel level ++ " - prod rule: " ++ show (productionRules !! prnum)) $ 
     debug flag (prlevel level ++ " - State " ++ show state) $ 
     debug flag (prlevel level ++ " - Stack " ++ prStack stk) $ 
     debug flag (prlevel level ++ " - Symbols: " ++ show symbols) $ 
     debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
     debug flag "" $ 

     if (rhsLength > length symbols) == False && False -- Q: 필요? False for the moment!
     then do return []

     else do let stk1 = drop (rhsLength*2) stk
             let topState = currentState stk1
             let toState = case lookupGotoTable (gotoTbl automaton) topState lhs of
                   Just state -> state
                   Nothing -> error $ "[simulReduce] Must not happen: lhs: "
                                      ++ lhs ++ " state: " ++ show topState
             let stk2 = push (StkNonterminal Nothing lhs) stk1  -- ast
             let stk3 = push (StkState toState) stk2

             if isSimple then  -- simple mode

               if isInitReduces searchState then -- reduces until symbols are found
                 do listOfList <- repReduce ccOption{cc_printLevel=level+1} [] toState stk3

                    let f syms0 (s, stk, syms) = (s, stk, syms0 ++ syms)

                    return (if null symbols
                            then listOfList
                            else {- (toState, stk3, symbols) : -} map (f symbols) listOfList)  -- Q: symbols: 필요?
               else if isFinalReduce searchState then
                 do return (if null symbols
                         then []
                         else [(toState, stk3, symbols)])
               else -- SS_GotoOrShift
                 do error $ "simulReduce: Unexpected search state" ++ show searchState

             else -- nested mode
               do error $ "simulReduce: Unexpected nested mode: "


simulGoto :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [Candidate]
 -> Int
 -> [StkElem token ast]
 -> IO [(State, Stack token ast, [Candidate])]
simulGoto ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in do -- debug flag $ prlevel level ++ "[simulGoto] " ++ show (cc_searchState ccOption)

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

                          debug flag (prlevel level ++ "GOTO [" ++ show i ++ "/" ++ show len ++ "] at "
                                         ++ show state ++ " -> " ++ show nonterminal ++ " -> " ++ show snext) $ 
                          debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                          debug flag (prlevel level ++ " - " ++ "Symbols:" ++ show (symbols++[NonterminalSymbol nonterminal])) $ 
                          debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
                          debug flag "" $ 

                          repGotoOrShift 
                            ccOption{cc_printLevel=level+1}
                              (symbols++[NonterminalSymbol nonterminal])
                                snext stk2)
                  (zip nontermStateList [1..])

              return $ concat listOfList

simulShift :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
 CompCandidates token ast
 -> [Candidate]
 -> Int
 -> [StkElem token ast]
 -> IO [(State, Stack token ast, [Candidate])]
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
  in do -- debug flag $ prlevel level ++ "[simulShift] " ++ show (cc_searchState ccOption)

        case cand2 of
         [] -> do return []

         _  -> do
                  listOfList <-
                    mapM (\ ((terminal,snext),i)-> 
                              let stk1 = push (StkTerminal (Terminal terminal 0 0 Nothing)) stk in
                              let stk2 = push (StkState snext) stk1 in

                              debug flag (prlevel level ++ "SHIFT [" ++ show i ++ "/" ++ show len ++ "]: "
                                            ++ show state ++ " -> " ++ terminal ++ " -> " ++ show snext) $ 
                              debug flag (prlevel level ++ " - " ++ "Stack " ++ prStack stk2) $ 
                              debug flag (prlevel level ++ " - " ++ "Symbols: " ++ show (symbols++[TerminalSymbol terminal])) $ 
                              debug flag (prlevel level ++ " - Search state: " ++ show (cc_searchState ccOption)) $ 
                              debug flag "" $ 

                              repGotoOrShift
                                ccOption{cc_printLevel=level+1}
                                  (symbols++[TerminalSymbol terminal])
                                    snext stk2)
                      (zip cand2 [1..])

                  return $ concat listOfList

repGotoOrShift
  :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
     CompCandidates token ast -> [Candidate] -> State -> Stack token ast -> IO [(State, Stack token ast, [Candidate])]

repGotoOrShift ccOption symbols state stk =
  let flag            = cc_debugFlag ccOption
      level           = cc_printLevel ccOption
      isSimple        = cc_simpleOrNested ccOption
      automaton       = cc_automaton ccOption
      
      actionTable     = actTbl automaton
      gotoTable       = gotoTbl automaton
      productionRules = prodRules automaton
  in do -- debug flag $ prlevel level ++ "[repGotoOrShift] " ++ show (cc_searchState ccOption)
    
        if null [True | ((s,lookahead),Accept) <- actionTable, state==s] == False
        then 
             debug flag (prlevel level ++ "accept: " ++ show state) $ 
             return []

        else do
                listOfList1 <- repReduce
                                 (ccOption{cc_searchState=
                                             SS_FinalReduce
                                               (r_level (cc_searchState ccOption))
                                               (gs_level (cc_searchState ccOption))})
                                   symbols state stk

                if null listOfList1 -- || isInitReduces (cc_searchState ccOption)
                  then
                       if gs_level (cc_searchState ccOption) - 1 > 0 then
                         let ccOption' = ccOption{cc_searchState=
                                             SS_GotoOrShift
                                               (r_level (cc_searchState ccOption))
                                               (gs_level (cc_searchState ccOption) - 1)}
                         in

                         -- both goto and shift only once 
                         if gs_level (cc_searchState ccOption) == cc_gs_level ccOption
                         then

                           do listOfList2 <- simulGoto ccOption' symbols state stk
                              listOfList3 <- simulShift ccOption' symbols state stk
                              return $ listOfList1 ++ listOfList2 ++ listOfList3

                         else

                           do listOfList2 <- simulGoto ccOption' symbols state stk

                              if null listOfList2
                              then

                                do listOfList3 <- simulShift ccOption' symbols state stk
                                   return $ listOfList1 ++ listOfList2 ++ listOfList3

                              else
                                return $ listOfList1 ++ listOfList2

                       else
                         return listOfList1  -- Q: symbols or []

                  else do return listOfList1

-- Todo: repReduce를 하지 않고
--       Reduce 액션이 있는지 보고
--       없으면 goto or shift 진행하고
--       있으면 reduce한번하고 종료!

--       현재 구현은 repReduce 결과가 널인지 검사해서 진행 또는 종료
--       Reduce 액션이 있어도 진행될 수 있음!



