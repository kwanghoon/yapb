module AutomatonType where

data Action = Shift Int | Reduce Int | Accept deriving (Eq, Show)

type ActionTable = [((Int, String), Action)] -- key: (Int,String), value: Action
type GotoTable   = [((Int, String), Int)]    -- key: (Int,String), value: Int
type ProdRules   = [(String, [String])]      -- key: Int,          value: (String, [String])

prProdRule :: (String, [String]) -> String
prProdRule (x,ys) =  x ++ " -> "  ++ pr_ys ys
  where
    pr_ys []     = ""  
    pr_ys [y]    = y 
    pr_ys (y:ys) = y ++ " " ++ pr_ys ys


