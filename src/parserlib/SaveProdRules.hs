module SaveProdRules where

import Text.Read (readMaybe)
import Data.Hashable
import System.IO
import System.Directory
import CFG

saveProdRules :: String -> String -> [String] -> String -> String -> String -> IO Bool
saveProdRules fileName startSymbol prodRuleStrs tokenAttrs prodRuleAttrs eot = do
  writeOnceWithHash fileName
    ("(" ++ grmStrLn ++ ",\n" ++
     tokenAttrs ++ ",\n" ++
     prodRuleAttrs ++ ",\n" ++
     show eot ++ ")\n")                  -- show eot to have double quotes
  where
    grmStr   = toCFG startSymbol prodRuleStrs
    grmStrLn = grmStr ++ "\n"

toCFG :: String -> [String] -> String {- CFG -}
toCFG startSymbol prodRuleStrs =
  "CFG " ++ show startSymbol ++
  " [\n" ++ concatWith (toProdRules prodRuleStrs) ",\n" ++ "\n ]"

toProdRules :: [String] -> [String] {- [ProductionRule] -}
toProdRules productionRuleStrs = map (toProdRule lhsStrs) lhsRhsStrss
  where
    lhsStrs     = map head lhsRhsStrss
    lhsRhsStrss = map tokenizeLhs productionRuleStrs
    
toProdRule :: [String] -> [String] -> String {- ProductionRule -}
toProdRule lhsStrs (lhs:rhsStrs) =
  " ProductionRule " ++ show lhs ++
  " [" ++ concatWith (map (toSymbol lhsStrs) rhsStrs) ", " ++ "]"

toSymbol :: [String] -> String -> String {- Symbol -}
toSymbol lhsStrs sym
  | sym `elem` lhsStrs = "Nonterminal " ++ show sym
  | otherwise          = "Terminal " ++ show sym

-- Parse production rules
tokenizeLhs :: String -> [String]
tokenizeLhs str =
  case lex str of
    []              -> error "No lhs found (1)"
    [("",therest)]  -> error "No lhs found (2)" 
    [(lhs,therest)] -> lhs : tokenizeArrow therest

tokenizeArrow :: String -> [String]
tokenizeArrow str =
  case lex str of
    []                     -> error "No arrow found (1)"
    [("",therest)]         -> error "No arrow found (2)" 
    [(arrow@"->",therest)] -> tokenizeRhs therest
    [(token,therest)]      -> error ("No arrow found: " ++ token)
    
tokenizeRhs :: String  -> [String]
tokenizeRhs str = words str  -- Terminals and nonterminls are space-separated words in RHS
  -- case lex str of
  --   []                -> []
  --   [("",therest)]    -> []
  --   [(token,therest)] -> token : tokenizeRhs therest

-- Utility
concatWith :: [String] -> String -> String
concatWith [] sep = ""
concatWith [a] sep = a
concatWith (a:b:theRest) sep = a ++ sep ++ concatWith (b:theRest) sep

getHashFileName fileName = fileName ++ ".hash"

writeOnceWithHash :: String -> String -> IO Bool
writeOnceWithHash fileName text = do
  let hashFileName = getHashFileName fileName
  let newHash = hash text
  
  fileExists <- doesFileExist fileName
  hashExists <- doesFileExist hashFileName

  case fileExists && hashExists of
    False -> do
      writeFile fileName text
      writeFile hashFileName (show newHash)
      return True

    True  -> do
      existingHashStr <- readFile hashFileName

      case readMaybe existingHashStr :: Maybe Int of
        Just existingHashStr_i ->
          
          case newHash == existingHashStr_i of
            True -> return False
            False -> do
              writeFile fileName text
              writeFile hashFileName (show newHash)
              return True

        Nothing -> error $ "[writeOnceWithHash] unexpected hash: " ++ existingHashStr
