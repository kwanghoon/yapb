module CFG where

import Data.List(nub,intersperse)

--------------------------------------------------------------------------------
-- Context Free Grammar
--------------------------------------------------------------------------------
data Symbol = Nonterminal String | Terminal String 
    deriving (Eq, Read)
             
instance Show Symbol where
  showsPrec p (Nonterminal x) = (++) x
  showsPrec p (Terminal x)    = (++) x
  
isTerminal (Terminal x) = True  
isTerminal _            = False
  
data ExtendedSymbol = Symbol Symbol | Epsilon | EndOfSymbol
    deriving Eq
             
instance Show ExtendedSymbol where
  showsPrec p (Symbol sym)    = (++) (show sym)
  showsPrec p (Epsilon)       = (++) "epsilon"
  showsPrec p (EndOfSymbol)   = (++) "$"
  
isExtendedTerminal (Symbol (Terminal x)) = True  
isExtendedTerminal (EndOfSymbol)         = True  
isExtendedTerminal _                     = False

isExtendedNonterminal (Symbol (Nonterminal x)) = True  
isExtendedNonterminal _                        = False

data ProductionRule = ProductionRule String [Symbol] 
         deriving (Eq, Read)
                  
instance Show ProductionRule where
  showsPrec p (ProductionRule x ys) = (++) x . (++) " -> " . show_ys ys
  
type ProductionRules = [ProductionRule]  
  
show_ys []     = (++) ""  
show_ys [y] = (++) (show y) 
show_ys (y:ys) = (++) (show y) . (++) " " . show_ys ys

data CFG = CFG String [ProductionRule] 
         deriving (Show, Read)

type AUGCFG = CFG

startNonterminal (CFG s prules) = s 

nonterminals augCfg = nub $ [s] ++ [x | ProductionRule x _ <- prules]
  where
    CFG s prules = augCfg

prodRuleToStr (ProductionRule s syms) =
  "ProductionRule " ++ show s
    ++  " [" ++ concat (intersperse ", " (map symbolToStr syms)) ++ "]"

symbolToStr (Nonterminal x) = "Nonterminal " ++ show x
symbolToStr (Terminal x) = "Terminal " ++ show x



