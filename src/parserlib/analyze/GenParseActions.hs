module GenParseActions (runYapbAutomatonWithLog) where


import Data.Typeable
import qualified Control.Monad.Trans.State.Lazy as ST

import Terminal
    ( Terminal(..),
      terminalToString,
      terminalToMaybeToken,
      tokenTextFromTerminal, terminalToTokenSymbol )
import TokenInterface ( TokenInterface(isEOT) )
import AutomatonType
    ( Action(Reduce, Accept, Shift),
      prProdRule,
      AutomatonTime(am_finishTime, am_cputime),
      AutomatonSpec(..) )
import ParserSpec ( LexerParserState )
import AutomatonStack
    ( StkElem(StkState, StkTerminal, StkNonterminal),
      emptyStack,
      push,
      prStack )
import AutomatonUtil
    ( currentState,
      lookupActionTable,
      lookupGotoTable,
      errorKeyword,
      lookupActionTableWithError,
      revTakeRhs )
import SynCompAlgoUtil ( debug )
import ParseError ( ParseError(NotFoundGoto, NotFoundAction) )
import ActionLogType 
import AnalyzeActions ( analyzeActions )

import Data.Maybe ( fromJust, isJust, isNothing )
import Control.Exception ( throw )
import System.IO
import Control.Monad.Cont (MonadIO(liftIO))

saveParserActions :: TokenInterface token => ActionLogs token -> ST.StateT (LexerParserState a) IO ()
saveParserActions logs =
  do h <- liftIO $ openFile "./action_logs" WriteMode
     liftIO $ save h logs
     liftIO $ hClose h
     liftIO $ analyzeActions logs
  where
    save h [] = return ()
    save h (action : logs) = do hPutStrLn h (showActionLog action) ; save h logs

-- 
runYapbAutomatonWithLog
  :: (TokenInterface token,
      Typeable token, Typeable ast, Typeable a,
      Show token, Show ast) =>
  -- debug flag
  Bool ->

  -- static part ActionTable,GotoTable,ProdRules,ParseActionList token ast ->
  AutomatonSpec token ast IO a ->

  -- dynamic part
  ST.StateT (LexerParserState a) IO (Terminal token) ->

  -- AST
  ST.StateT (LexerParserState a) IO (ActionLogs token)

runYapbAutomatonWithLog flag am_spec@AutomatonSpec {
      am_initState=initState,
      am_actionTbl=actionTbl,
      am_gotoTbl=gotoTbl,
      am_prodRules=prodRules,
      am_parseFuns=pFunList
   } nextTerminal =

      do let initStack = push (StkState initState) emptyStack
         logs <- run Nothing initStack Nothing
         saveParserActions logs
         return logs

  where
    {- run :: TokenInterface token => [Terminal token] -> Stack token ast -> IO ActionLog -}
    run maybeTerminal stack _maybeStatus = do
      let state = currentState stack
      -- let terminal = head terminalList

      -- Save the current state in case of going back
      prevState <- ST.get

      terminal <- maybe nextTerminal return maybeTerminal      -- Use a terminal multiple times
                                                               -- when reducing multiple production rules

      maybeStatus <-
            -- if isNothing _maybeStatus && length terminalList == 1   -- if terminal == "$"
            if isNothing _maybeStatus
               && isJust (terminalToMaybeToken terminal)
               && isEOT (fromJust (terminalToMaybeToken terminal))
            then debug flag "" $
                 debug flag ("Saving: " ++ prStack stack) $
                 do return (Just stack)
            else do return _maybeStatus

      case lookupActionTable actionTbl state terminal of
        Just action -> do
          -- putStrLn $ terminalToString terminal {- debug -}
          runAction state terminal action stack maybeStatus

        Nothing ->
          debug flag ("lookActionTable failed (1st) with: " ++ show (terminalToString terminal)) $

          case lookupActionTableWithError actionTbl state of
            Just action -> do
              -- errorTerminal intended to share the same (line,col) as terminal
              let (_, line, col, _) = prevState
              let errorTerminal     = Terminal errorKeyword line col Nothing

              -- Restore the current state
              ST.put prevState

              -- run this action (Shift toState)
              runAction state errorTerminal action stack maybeStatus

            Nothing ->
              -- No more way to proceed now!
              do lp_state <- ST.get
                 am_finishTime (am_time am_spec) (am_cputime (am_time am_spec))
                 throw (NotFoundAction terminal state stack actionTbl gotoTbl prodRules lp_state maybeStatus)

    -- separated to support the haskell layout rule
    runAction state terminal action stack maybeStatus =
      debug flag ("\nState " ++ show state) $
      debug flag ("Token " ++ tokenTextFromTerminal terminal) $
      debug flag ("Stack " ++ prStack stack) $

      case action of
        Accept ->
          debug flag "Accept" $
          debug flag (terminalToString terminal) $ {- debug -}

          case stack !! 1 of
            StkNonterminal (Just ast) _ ->
              do am_finishTime (am_time am_spec) (am_cputime (am_time am_spec))
                 return [LogAccept]
            StkNonterminal Nothing _ -> error "Empty ast in the stack nonterminal"
            _ -> error "Not Stknontermianl on Accept"

        Shift toState ->
            debug flag ("Shift " ++ show toState) $
            debug flag (terminalToString terminal) $ {- debug -}

            let stack1 = push (StkTerminal terminal) stack in
            let stack2 = push (StkState toState) stack1 in
            do logs <- run Nothing stack2 maybeStatus  -- Nothing means consuming the terminal!
               return $ LogShift toState terminal : logs

        Reduce n ->
            debug flag ("Reduce " ++ show n) $

            let prodrule = prodRules !! n in
            let prodruleText = prProdRule prodrule in
            debug flag ("\t" ++ prodruleText) $

            let builderFun = pFunList  !! n in
            let lhs        = fst prodrule in
            let rhsLength  = length (snd prodrule) in
            let rhsAst = revTakeRhs rhsLength stack in
            do ast <- builderFun rhsAst
               let stack1 = drop (rhsLength*2) stack
               let topState = currentState stack1

               toState <-
                    case lookupGotoTable gotoTbl topState lhs of
                      Just state -> return state
                      Nothing -> do lp_state <- ST.get
                                    am_finishTime (am_time am_spec) (am_cputime (am_time am_spec))
                                    throw (NotFoundGoto topState lhs stack
                                             actionTbl gotoTbl prodRules
                                               lp_state maybeStatus)

               let stack2 = push (StkNonterminal (Just ast) lhs) stack1
               let stack3 = push (StkState toState) stack2
               logs <- run (Just terminal) stack3 maybeStatus -- Use the terminal again the next time!
               return $ LogReduce n prodruleText rhsLength : LogGoto toState lhs : logs