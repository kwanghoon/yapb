module AutomatonType where

data Action = Shift Int | Reduce Int | Accept deriving Eq

type ActionTable = [((Int, String), Action)] -- key: (Int,String), value: Action
type GotoTable   = [((Int, String), Int)]    -- key: (Int,String), value: Int
type ProdRules   = [(String, [String])]      -- key: Int,          value: (String, [String])

