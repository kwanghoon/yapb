module SampleGrammar where

import CFG

--------------------------------------------------------------------------------
-- [Sample CFG Grammar] : g1 from Example 4.33 in the Dragon book (2nd Ed.)
--------------------------------------------------------------------------------
g1 = CFG "E'" [p0,p1,p2,p3,p4,p5,p6]

-- E' -> E
p0 = ProductionRule "E'" [Nonterminal "E"]

-- E -> E + T
p1 = ProductionRule "E" [Nonterminal "E", Terminal "+", Nonterminal "T"] 

-- E -> T
p2 = ProductionRule "E" [Nonterminal "T"]

-- T -> T * F
p3 = ProductionRule "T" [Nonterminal "T", Terminal "*", Nonterminal "F"]

-- T -> F
p4 = ProductionRule "T" [Nonterminal "F"]

-- F -> ( E )
p5 = ProductionRule "F" [Terminal "(", Nonterminal "E", Terminal ")"]

-- F -> id
p6 = ProductionRule "F" [Terminal "id"]

--------------------------------------------------------------------------------
-- [Sample CFG Grammar] : g2 from Example 4.2 in the Dragon book (2nd Ed.)
--------------------------------------------------------------------------------
g2 = CFG "S'" [q1,q2,q3,q4]

q1 = ProductionRule "S'" [Nonterminal "S"]
q2 = ProductionRule "S" [Nonterminal "C", Nonterminal "C"]
q3 = ProductionRule "C" [Terminal "c", Nonterminal "C"]
q4 = ProductionRule "C" [Terminal "d"]

--------------------------------------------------------------------------------
-- [Sample CFG Grammar] : g3 from the LF calculus
--------------------------------------------------------------------------------
g3 = CFG "S'" [lfp0,lfp1,lfp2,lfp5,lfp6,lfp7,lfp8,lfp9,lfp10,lfp11
              ,lfp12,lfp13,lfp14,lfp15,lfp16,lfp17,lfp18,lfp19,lfp20,lfp21
              ,lfp22,lfp23,lfp24,lfp25,lfp26,lfp27,lfp28,lfp29,lfp30,lfp31]

lfp0 = ProductionRule "S'" [Nonterminal "Program"]
lfp1 = ProductionRule "Program" [Nonterminal "Decl"]

lfp2 = ProductionRule "Decl" [Nonterminal "TypeDeclaration", 
                              Nonterminal "TermDeclaration", 
                              Nonterminal "DefDeclaration"]

lfp5 = ProductionRule "TypeDeclaration" 
       [Terminal "atType", Nonterminal "TyDecls"]
lfp6 = ProductionRule "TermDeclaration"
       [Terminal "atTerm", Nonterminal "TmDecls"]
lfp7 = ProductionRule "DefDeclaration" []
lfp8 = ProductionRule "DefDeclaration"
       [Terminal "atDef", Nonterminal "DefDecls"]
       
lfp9 = ProductionRule "TyDecls"
       [Terminal "var", Terminal ":", Nonterminal "K", Terminal "." ]
lfp10 = ProductionRule "TyDecls"
       [Terminal "var", Terminal ":", Nonterminal "K", Terminal "."
       , Nonterminal "TyDecls" ]
       
lfp11 = ProductionRule "TmDecls"
       [Terminal "var", Terminal ":", Nonterminal "A", Terminal "." ]
lfp12 = ProductionRule "TmDecls"
       [Terminal "var", Terminal ":", Nonterminal "A", Terminal "."
       , Nonterminal "TmDecls" ]
       
lfp13 = ProductionRule "DefDecls"
       [Terminal "var", Terminal "=", Nonterminal "M", Terminal "." ]
lfp14 = ProductionRule "DefDecls"
       [Terminal "var", Terminal "=", Nonterminal "M", Terminal "."
       , Nonterminal "DefDecls" ]
       
lfp15 = ProductionRule "K" [Terminal "Type"]
lfp16 = ProductionRule "K" [Terminal "Pi", Terminal "var", Terminal ":"
                           , Nonterminal "A", Terminal ".", Nonterminal "K"]
lfp17 = ProductionRule "K" [Terminal "(", Nonterminal "K", Terminal ")"]        
lfp18 = ProductionRule "K" [Nonterminal "A1", Terminal "arrow", Nonterminal "K"]

lfp19 = ProductionRule "A" [Terminal "Pi", Terminal "var", Terminal ":"
                           , Nonterminal "A", Terminal ".", Nonterminal "A"]
lfp20 = ProductionRule "A" [Nonterminal "A1"]        
lfp21 = ProductionRule "A" [Nonterminal "A1", Terminal "arrow", Nonterminal "A"]

lfp22 = ProductionRule "A1" [Terminal "var"]
lfp23 = ProductionRule "A1" [Terminal "(", Nonterminal "A", Terminal ")"]
lfp24 = ProductionRule "A1" [Nonterminal "A1", Terminal "var"]
lfp25 = ProductionRule "A1" [Nonterminal "A1", Terminal "(", Nonterminal "M"
                            , Terminal ")"]
        
lfp26 = ProductionRule "M" [Terminal "Lam", Terminal "var", Terminal ":", 
                            Nonterminal "A", Terminal ".", Nonterminal "M"]
lfp27 = ProductionRule "M" [Nonterminal "M1"]

lfp28 = ProductionRule "M1" [Terminal "var"]
lfp29 = ProductionRule "M1" [Terminal "(", Nonterminal "M", Terminal ")"]
lfp30 = ProductionRule "M1" [Nonterminal "M1", Terminal "var"]
lfp31 = ProductionRule "M1" [Nonterminal "M1", Terminal "(", Nonterminal "M",
                             Terminal ")"]
        
type SemRuleName = String
data SemanticRule = SemanticRule SemRuleName [Int]
    
lfs0 = SemanticRule "DoNothing" []
lfs1 = SemanticRule "DoNothing" []
lfs2 = SemanticRule "DoNothing" []
lfs5 = SemanticRule "DoNothing" []
lfs6 = SemanticRule "DoNothing" []
lfs7 = SemanticRule "DoNothing" []
lfs8 = SemanticRule "DoNothing" []

lfs9 = SemanticRule "DeclK" [1,3]
lfs10 = SemanticRule "DeclK" [1,3]
lfs11 = SemanticRule "DeclA" [1,3]
lfs12 = SemanticRule "DeclA" [1,3]
lfs13 = SemanticRule "DeclM" [1,3]
lfs14 = SemanticRule "DeclM" [1,3]

lfs15 = SemanticRule "MkType" []
lfs16 = SemanticRule "MkPiK" [2,4,6]
lfs17 = SemanticRule "ReturnK" [2]
lfs18 = SemanticRule "MkArrowK" [1,3]
lfs19 = SemanticRule "MkPiA" [2,4,6]
lfs20 = SemanticRule "ReturnA" [1]
lfs21 = SemanticRule "MkArrowA" [1,3]
lfs22 = SemanticRule "MkName" [1]
lfs23 = SemanticRule "ReturnA" [2]
lfs24 = SemanticRule "MkAppA" [1,2]
lfs25 = SemanticRule "MkAppA" [1,3]
lfs26 = SemanticRule "MkLamM" [2,4,6]
lfs27 = SemanticRule "ReturnM" [1]
lfs28 = SemanticRule "MkName" [1]
lfs29 = SemanticRule "ReturnM" [2]
lfs30 = SemanticRule "MkAppM" [1,2]
lfs31 = SemanticRule "MkAppM" [1,3]

-- The attributes of terminals in g3
g3_attrib_terminals =
  [ ("Type",   "TYPE")
  , ("Pi",     "PI")
  , ("Lam",    "LAM")
  , (":",      "COLON")
  , (".",      "DOT")
    
  , ("(",      "OPEN")
  , (")",      "CLOSE")
  , ("=",      "EQ")
  , ("arrow",  "ARROW")
  , ("atType", "ATTYPE")
  
  , ("atTerm", "ATTERM")
  , ("atDef",  "ATDEF")  
  , ("var",    "VAR")
  , ("num",    "NUM")
  ]



g4 :: CFG
g4 = CFG "S'" [ g4_0, g4_s1, g4_s2, g4_l1, g4_l2, g4_r ]
  where
    g4_0 = ProductionRule "S'" [Nonterminal "S"]
    g4_s1 = ProductionRule "S" [ Nonterminal "L", Terminal "=", Nonterminal "R" ]
    g4_s2 = ProductionRule "S" [ Nonterminal "R" ]
    g4_l1 = ProductionRule "L" [ Terminal "*", Nonterminal "R" ]
    g4_l2 = ProductionRule "L" [ Terminal "id" ]
    g4_r = ProductionRule "R" [ Nonterminal "L" ]