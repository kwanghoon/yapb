module MaxRhsLen (maxRhsLen,maxRhsLen_) where

import CFG
import Attrs

import Data.List (sort)

-- | Calculate the maximum length of RHSs from mygrammar.grm

maxRhsLen :: String -> IO Int
maxRhsLen file =
  do text <- readFile file
     let s = read text :: (CFG,TokenAttrs,ProdRuleAttrs,String)
     return (last (sort [ length rhs | ProductionRule _ rhs <- pr s ]))
  where pr (CFG _ prodrules,_,_,_) = prodrules


-- | Calculate the maximum length of RHSs from the text grammar (Temporary)

maxRhsLen_ :: String -> IO Int
maxRhsLen_ file =
  do text <- readFile file
     return (last (sort [length (drop 3 list) | list <- map words (lines text)]))


-- | Examples

-- ghci>maxRhsLen "/home/khchoi/work/lang/haskell/sbparser/mygrammar.grm"
-- 9

-- ghci>maxRhsLen "/home/khchoi/work/lang/haskell/polyrpc/mygrammar.grm"
-- 8

-- ghci>maxRhsLen "/home/khchoi/work/lang/haskell/c11parser/mygrammar.grm"
-- 9

-- ghci>maxRhsLen_ "/home/khchoi/work/lang/haskell/happy/hslexer/haskell_parser_grammar.txt"
-- 9
