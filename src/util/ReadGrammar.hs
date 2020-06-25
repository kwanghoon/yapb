module ReadGrammar where

import CFG

import Data.List(intersperse)
import System.IO
import System.Environment (getArgs)

data LitGrm = LitGrm { start :: Maybe String, rules :: [(String, [[String]])], rhss :: [[String]] }

readGrammar :: Monad m => [String] -> m (Maybe String, [ProductionRule])
readGrammar lines = do
  startLhsRhssPairList <- rep NoState lines
  let startsymbol = start startLhsRhssPairList
  let lhsRhssPairList = rules startLhsRhssPairList
  let nonterminals = map fst lhsRhssPairList
  return (startsymbol, concat (map (convert nonterminals) lhsRhssPairList))

-- Checking
convert :: [String] -> (String, [[String]]) -> [ProductionRule]
convert nonterminals (lhs, rhss) =
  map (\rhs -> ProductionRule lhs
               (map (\s -> if s `elem` nonterminals
                           then Nonterminal s
                           else Terminal s) rhs)) rhss

-- Parsing
data State =
    NoState
  | StartSymbol String
  | Lhs String
  | Rhs [[String]]
  deriving Eq

-- Note
--  * take the first word. After that, it may be regarded as a comment.
begin :: Monad m => [Char] -> m State
begin [] = return NoState
begin ('@':'s':'t':'a':'r':'t':' ':cs) = return (StartSymbol (takeWord cs))
begin (';':cs) = return NoState
begin (' ':' ':'=':[]) = return (Rhs [[]])
begin (' ':' ':'=':' ':cs) = return (Rhs [words cs])
begin (' ':' ':'|':' ':cs) = return (Rhs [words cs])
begin cs =
  let w = takeWord cs in
    case w of
      [] -> return NoState
      _  -> return (Lhs w)

takeWord :: String -> String
takeWord []        = []
takeWord (' ':cs)  = []
takeWord ('\t':cs) = []
takeWord (c:cs)    = c : takeWord cs

rep :: Monad m => State -> [String] -> m LitGrm
rep (Lhs lhs) []     = error "rep: Can't end with Lhs"
rep (_)       []     = return $ LitGrm {start=Nothing, rules=[], rhss=[]}
rep prestate  (s:ss) = do
  state <- begin s
  startLhsRhsPairList <- rep state ss
  case (prestate, state) of
    (NoState, NoState) -> return startLhsRhsPairList
    (NoState, StartSymbol s) -> 
      case start startLhsRhsPairList of
        Just s' -> error $ "rep: StartSymbol duplicated: " ++ s ++ ", " ++ s'
        Nothing -> return startLhsRhsPairList {start = Just s}
    (NoState, Lhs lhs) ->
      let rules_ = rules startLhsRhsPairList
          rhss_  = rhss startLhsRhsPairList
      in return startLhsRhsPairList { rules=(lhs,rhss_):rules_, rhss=[] }
    (NoState, Rhs rhss) -> error "rep: Nostate can't change to Rule lhs rhss."
    
    (Lhs lhs, NoState) -> error $ "rep: Lhs " ++ lhs ++ " can't change to Nostate."
    (Lhs lhs, StartSymbol s) -> error $ "rep: Lhs " ++ lhs ++ " can't change to StartSymbol " ++ s
    (Lhs lhs, Lhs lhs') -> error $ "rep: Lhs " ++ lhs ++ " can't change to " ++ lhs'
    (Lhs lhs, Rhs rhss_) ->
      let rhss__ = rhss startLhsRhsPairList
      in  return startLhsRhsPairList {rhss = rhss_ ++ rhss__}
    
    (Rhs rhss, NoState) -> return startLhsRhsPairList
    (Rhs rhss, StartSymbol s) -> error $ "rep: Rhs can't change to StartSymbol " ++ s
    (Rhs _, Lhs _) -> error "rep: Rhs can't change to Lhs lhs."
    (Rhs _, Rhs rhss_) ->
      let rhss__ = rhss startLhsRhsPairList
      in  return startLhsRhsPairList {rhss = rhss_ ++ rhss__}
    
    (StartSymbol s, NoState) -> return startLhsRhsPairList
    (StartSymbol s, StartSymbol s') -> error $ "rep: StartSymbol duplicated(4): " ++ s ++ ", " ++ s'
    (StartSymbol s, Lhs lhs) ->
      let rules_ = rules startLhsRhsPairList
          rhss_  = rhss startLhsRhsPairList
      in return startLhsRhsPairList { rules=(lhs,rhss_):rules_, rhss=[] }
    (StartSymbol s, Rhs rhss) -> error $ "rep: StartSymbol " ++ s ++ " can't change to Rule"

----
-- For testing with grm/polyrpc.lgrm
-- 

test fun = do
  args <- getArgs
  repTest fun args

repTest fun [] = return ()
repTest fun (arg:args) = do
  text <- readFile arg
  fun text
  repTest fun args

parsing text = do
  startLhsRhssPairList <- rep NoState (lines text)
  let startsymbol = start startLhsRhssPairList
  let lhsRhssPairList = rules startLhsRhssPairList
  mapM_ (\(lhs,rhss) -> prLhsRhss lhs rhss) lhsRhssPairList

prLhsRhss :: String -> [[String]] -> IO ()
prLhsRhss lhs rhss = do
  putStrLn lhs
  mapM_ (\rhs ->
         do { putStr "\t"
            ; mapM_ (\s -> do {putStr s; putStr " "}) rhs
            ; putStrLn ""} )  rhss

conversion text = do
  (startsymbol_, prodrules_) <- readGrammar (lines text)
  case startsymbol_ of
    Nothing -> error "conversion: No start symbol"
    Just startsymbol ->
      do
        let startsymbol' = startsymbol ++ "'"
        let startprod = ProductionRule startsymbol' [ Nonterminal startsymbol ]
        let prodrules = startprod : prodrules_
        putStr $ "CFG " ++ show startsymbol' ++ " [\n "
        -- May replace prodRuleToStr with show
        putStrLn $ concat (intersperse ",\n " (map prodRuleToStr prodrules))  
        putStrLn $ "]"
    
