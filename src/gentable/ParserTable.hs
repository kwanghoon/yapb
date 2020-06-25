module ParserTable where

import CFG

import System.IO

-- LR(1) item
data Item = Item ProductionRule Int [ExtendedSymbol] {- except Epsilon -}
            deriving Eq
                     
type Items  = [Item]
type Itemss = [Items]

instance Show Item where
  showsPrec p (Item (ProductionRule x syms) j [])
    = (++) "[" 
      . (++) x
      . (++) " -> "
      . show_ys (take j syms)
      . (++) "." 
      . show_ys (drop j syms)
      . (++) "]"
  showsPrec p (Item (ProductionRule x syms) j [esym])
    = (++) "[" 
      . (++) x
      . (++) " -> "
      . show_ys (take j syms)
      . (++) "." 
      . show_ys (drop j syms)
      . (++) ", "
      . (++) (show esym)
      . (++) "]"
      
prItem :: Handle -> Items -> IO ()
prItem h xs = do  prItem' h xs
                  hPutStrLn h ""
  where
    prItem' h []     = return ()
    prItem' h (x:xs) = do hPutStrLn h (show x)
                          prItem' h xs
    
  
prItems :: Handle -> Itemss -> IO ()
prItems h xs = prItems' h 0 xs

prItems' h n []       = return ()
prItems' h n (is:iss) =
  do hPutStrLn h ("I" ++ show n ++ ":")
     prItem h is
     prItems' h (n+1) iss


isKernel :: String -> Item -> Bool
isKernel startnonterminal (Item (ProductionRule lhs rhs) dot lookahead) =
  dot /= 0 || startnonterminal == lhs

-- LR(1) Table             
data Action = Shift Int | Reduce Int | Accept | Reject
            deriving (Show, Eq)
                     
type ActionTable = [(Int, ExtendedSymbol, Action)] -- state, terminal, action
type GotoTable   = [(Int, Symbol, Int)]    -- state, nonterminal, state

lookupTable :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Maybe c
lookupTable i x [] 
  = Nothing 
lookupTable i x ((j,y,a):tbl)
  = if i == j && x == y then Just a 
    else lookupTable i x tbl
    
prActTbl h [] = return ()
prActTbl h ((i,x,a):actTbl) = 
  do hPutStrLn h (show i ++ "\t" ++ show x ++ "\t" ++ show a)
     prActTbl h actTbl
     
prGtTbl h [] = return ()     
prGtTbl h ((i,x,j):gtTbl) =
  do hPutStrLn h (show i ++ "\t" ++ show x ++ "\t" ++ show j)
     prActTbl h gtTbl


-- LALR(1) Table
data LALRAction = LALRShift [Int] | LALRReduce Int | LALRAccept | LALRReject
            deriving (Show, Eq)
                     
type LALRActionTable = [([Int], ExtendedSymbol, LALRAction)]
type LALRGotoTable   = [([Int], Symbol, [Int])]