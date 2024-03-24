--------------------------------------------------------------------------------
-- An LR Parser Table Generator
-- 
-- Copyright(c) 2013 Kwanghoon Choi. All rights reserved.
--
-- Usage:
--  $ ghci GenLRParserTable
--  *Main> prParseTable (calcLR1ParseTable g1)
--  *Main> prLALRParseTable (calcLALRParseTable g1)
--  *Main> prItemsFrom g1
--
--  * let (items,_,lkhtbl,gotos) = calcLR0ParseTable g1 
--    in do { prItems items; prGtTbl gotos; prLkhTable lkhtbl }
--
--  * closure g4 [Item (ProductionRule "S'" [Nonterminal "S"]) 0 [Symbol (Terminal "")]]
--------------------------------------------------------------------------------

module GenLRParserTable (_main) where

import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import System.Environment (getArgs)

import CFG
import Attrs
import ParserTable
import CmdArgs 

import System.IO

----------------------------------------------------------------------------------------------------
-- | [Usage] YAPB as a command-line
----------------------------------------------------------------------------------------------------

-- 1) Generate LALR parser automation from one or more grammar files

--    $ stack exec -- yapb-exe rpc.grm 
--    $ stack exec -- yapb-exe rpc.grm smallbasic.grm      

-- 2) Write LALR parser automation from one or more grammar files into specified files with -output
--    (Note: only one grm file must be specified with -output option)

--    $ stack exec -- yapb-exe  rpc.grm -output prod_rules.txt action_table.txt goto_table.txt  
--    $ stack exec -- yapb-exe  -output prod_rules.txt action_table.txt goto_table.txt  rpc.grm
--    $ stack exec -- yapb-exe  -output prod_rules.txt action_table.txt goto_table.txt  rpc.grm

-- 3) Show LALR items from one or more grammar files

--    $ stack exec -- yapb-exe -show-items rpc.grm
--    $ stack exec -- yapb-exe -show-items rpc.grm smallbasic.grm

-- 4) Show simple structural candidates from LALR items

--    $ stack exec -- yapb-exe -show-candidates rpc.grm
--    $ stack exec -- yapb-exe -show-candidates rpc.grm smallbasic.grm

----------------------------------------------------------------------------------------------------

_main = do
  args <- getArgs
  -- mapM_ putStrLn args
  let cmd = getCmd args 
  case cmd of 
    CmdError msg -> putStrLn msg
    CmdGrmFiles fileNames -> mapM_ (f stdout) fileNames 
    CmdShowItems fileNames -> mapM_ (prItemsFrom stdout) fileNames 
    CmdShowCandidates fileNames -> mapM_ (prCandidatesFrom stdout) fileNames 
    CmdGrmWithOption (Just fileName) prod_rule action_tbl goto_tbl -> do
      writeParseTable fileName prod_rule action_tbl goto_tbl
      putStrLn "Done"

  where
    f h file = do
      grammar <- readFile file
      -- putStrLn grammar
      let (cfg,tokenAttrs,prodRuleAttrs,eot) =
            case readMaybe grammar :: Maybe (CFG, TokenAttrs, ProdRuleAttrs,String) of
              Just ctp -> ctp
              Nothing -> error $ "[GenLRParserTable:_main:f] unexpected "
                                    ++ "cfg, token attrs, and prod rule attrs"

      (items, prules, actTbl, gtTbl, conflictsResolved)
            <- calcEfficientLALRParseTable cfg eot tokenAttrs
                 (setProdRuleAttrs cfg tokenAttrs prodRuleAttrs)

      prConflictsResolved conflictsResolved

      prParseTable stdout (items, prules, actTbl, gtTbl)

    writeParseTable file prod_rule action_tbl goto_tbl =
      do
        grammar <- readFile file
        let (cfg,tokenAttrs,prodRuleAttrs,eot) =
              case readMaybe grammar :: Maybe (CFG, TokenAttrs, ProdRuleAttrs,String) of
                Just cfp -> cfp
                Nothing -> error $ "[GenLRParserTable:writeParseTable] unexpected "
                                      ++ "cfg, token attrs, prod rule attrs, and eot"
              
        (items, prules, actTbl, gtTbl, conflictsResolved)
              <- calcEfficientLALRParseTable cfg eot tokenAttrs
                   (setProdRuleAttrs cfg tokenAttrs prodRuleAttrs)

        prConflictsResolved conflictsResolved

        h_pr <- openFile prod_rule WriteMode
        h_acttbl <- openFile action_tbl WriteMode 
        h_gototbl <- openFile goto_tbl WriteMode

        prPrules h_pr prules
        prActTbl h_acttbl actTbl
        prGtTbl h_gototbl gtTbl

        hClose h_pr
        hClose h_acttbl 
        hClose h_gototbl

setProdRuleAttrs cfg _tokenAttrs _initProdRuleAttrs =
  ProdRuleAttrs $
    concat [ getAssocPrec no rhs | (no, ProductionRule _ rhs) <- zip [0..] prodRules ]
  where
    CFG ss prodRules = cfg
    TokenAttrs tokenAttrs = _tokenAttrs
    ProdRuleAttrs initProdRuleAttrs = _initProdRuleAttrs

    getAssocPrec no rhs =
      case [ (assoc,prec) | (no',(assoc,prec)) <- initProdRuleAttrs, no==no' ] of
        ((assoc,prec):_) -> [ (no, (assoc,prec)) ]
        [] -> findRightmostTerminal no (reverse rhs)

    findRightmostTerminal no [] = []
    findRightmostTerminal no (Nonterminal _ : rhs) = findRightmostTerminal no rhs
    findRightmostTerminal no (Terminal tok : rhs) =
      case [ (assoc,prec) | (tok', (assoc,prec)) <- tokenAttrs, tok==tok' ] of
        ((assoc,prec):_) -> [ (no, (assoc,prec)) ]
        [] -> []  

-- __main g = do
--   prParseTable stdout $ (\(a1,a2,a3,a4,a5)->(a1,a2,a3,a4)) (calcEfficientLALRParseTable g)

-- __mainDebug g = do
--   let (_,_,_,_,(items,lkhtbl1,splk',lkhtbl2,gotos)) = calcEfficientLALRParseTable g
--   let kernelitems = map (filter (isKernel (startNonterminal g))) items
--   prItems items
--   prGtTbl gotos
--   prItems kernelitems
--   putStrLn "closure with #"
--   let f (i, x,y) = do { putStrLn (show i ++ " : " ++ show x); prItem y; putStrLn "" }
--   mapM_ f $ [ (index, item, closure g [Item prule dot [sharpSymbol]])
--             | (index,items) <- zip [0..] kernelitems
--             , item@(Item prule dot _) <- items ]
--   putStrLn "Splk'"
--   prSplk' splk'
--   putStrLn "Splk:"
--   prSpontaneous lkhtbl1
--   putStrLn "Prop:"
--   prPropagate lkhtbl2 
--   putStrLn ""
--   prItems (computeLookaheads lkhtbl1 lkhtbl2 kernelitems)

prSplk' [] = return ()
prSplk' ((index0,index2,item0,item0closure,item1,item2):splk') = do
  putStrLn "item0:"
  putStrLn (show index0)
  putStrLn (show item0)
  putStrLn "closure(item0,#):"
  prItem stdout item0closure
  putStrLn "item1:"
  putStrLn (show item1)
  putStrLn (show index2)
  putStrLn "item2:"
  putStrLn (show item2)
  ch <- getChar
  prSplk' splk'

-- __mainLr1 g = do
--   prParseTable stdout (calcLR1ParseTable g)

-- __mainLalr1 g = do   
--   prLALRParseTable stdout (calcLALRParseTable g)

prItemsFrom h gFile =
   do grammarInfo <- readFile gFile

      let (cfg, tokenAttrs, prodRuleAttrs, eot) =
            case readMaybe grammarInfo :: Maybe (CFG, TokenAttrs, ProdRuleAttrs, String) of
              Just ctp -> ctp
              Nothing -> error $ "[GenLRParserTable:prItemsFrom] unexpected "
                                    ++ "cfg, token attrs, and prod rules attrs"

      (items, _, _, _, _) <- calcEfficientLALRParseTable cfg eot tokenAttrs
                                  (setProdRuleAttrs cfg tokenAttrs prodRuleAttrs)

      hPutStrLn h (show (length items) ++ " states")
      prItems stdout items

prCandidatesFrom h gFile =
   do grammarInfo <- readFile gFile

      let (cfg, tokenAttrs, prodRuleAttrs, eot) =
            case readMaybe grammarInfo :: Maybe (CFG, TokenAttrs, ProdRuleAttrs, String) of
              Just ctp -> ctp
              Nothing -> error $ "[GenLRParserTable:prItemsFrom] unexpected "
                                    ++ "cfg, token attrs, and prod rules attrs"

      (itemss, prules, _, _, _) <- calcEfficientLALRParseTable cfg eot tokenAttrs
                                    (setProdRuleAttrs cfg tokenAttrs prodRuleAttrs)

      hPutStrLn h (show (length itemss) ++ " states")
      prCandidates stdout itemss prules     

prCandidates h itemss prules = mapM_ prC $ zip [0..] itemss
   where
      prC (state, items) = 
        do hPutStrLn h ("I" ++ show state ++ ":")
           mapM_ prI $ nubBy f items
           hPutStrLn h ""

      f (Item prodRule1 pos1 _) (Item prodRule2 pos2 _) =
        prodRule1 == prodRule2 && pos1 == pos2

      prI (Item prodRule pos _) = 
       hPutStrLn h $ show n ++ " " ++ concat (map tOrN candidate)
        where ProductionRule lhs rhsSymbols = prodRule
              n = lookup prodRule
              candidate = drop pos rhsSymbols
              tOrN (Terminal t) = "T " ++ t ++ " "
              tOrN (Nonterminal nt) = "N " ++ nt ++ " "

      nPrules = zip [0..] prules

      lookup prule = 
        case [ n | (n, prule') <- nPrules, prule==prule'] of
          n:_ -> n
          _   -> error "prCandidates: lookup: something wrong"

--
indexPrule :: AUGCFG -> ProductionRule -> Int
indexPrule augCfg prule = indexPrule' prules prule 0
  where
    CFG _ prules = augCfg
  
indexPrule' []     prule n = error ("indexPrule: not found " ++ show prule)
indexPrule' (r:rs) prule n = 
  if r == prule then n else indexPrule' rs prule (n+1)
                            
prPrules h ps = prPrules' h ps 0

prPrules' h [] n = return ()
prPrules' h (prule:prules) n = 
  do hPutStrLn h (show n ++ ": " ++ show prule)
     prPrules' h prules (n+1)
      
--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------
symbols :: CFG -> [Symbol]
symbols (CFG start prules) 
  = [Nonterminal x | Nonterminal x <- syms] ++
    [Terminal x    | Terminal x    <- syms]
  where
    f (ProductionRule x syms) = Nonterminal x:syms
    syms = nub (Nonterminal start : concat (map f prules))

--
first :: [(Symbol, [ExtendedSymbol])] -> Symbol -> [ExtendedSymbol]
first tbl x = case (lookup x tbl) of
  Nothing -> [Symbol x]
  -- Nothing -> if x == Terminal "#" 
  --             then [Symbol x] 
  --             else error (show x ++ " not in " ++ show tbl)
  Just y -> y

first_ :: [(Symbol, [ExtendedSymbol])] -> [Symbol] -> [ExtendedSymbol]
first_ tbl []     = []
first_ tbl (z:zs) = let zRng = first tbl z in
  if elem Epsilon zRng 
  then union ((\\) zRng [Epsilon]) (first_ tbl zs)
  else zRng
                                                            
extFirst :: [(Symbol, [ExtendedSymbol])] -> ExtendedSymbol -> [ExtendedSymbol]
extFirst tbl (Symbol x)        = first tbl x
extFirst tbl (EndOfSymbol eot) = [EndOfSymbol eot]
extFirst tbl (Epsilon)         = error "extFirst_ : Epsilon"

extFirst_ :: [(Symbol, [ExtendedSymbol])] -> [ExtendedSymbol] -> [ExtendedSymbol]
extFirst_ tbl []     = []
extFirst_ tbl (z:zs) = let zRng = extFirst tbl z in
  if elem Epsilon zRng 
  then union ((\\) zRng [Epsilon]) (extFirst_ tbl zs)
  else zRng
  
--
calcFirst :: CFG -> [(Symbol, [ExtendedSymbol])]
calcFirst cfg = calcFirst' cfg (initFirst cfg) (symbols cfg)
    
initFirst cfg =
  let syms         = symbols cfg
      CFG _ prules = cfg
  in [(Terminal x, [Symbol (Terminal x)]) 
     | Terminal x <- syms]
     ++    
     [(Nonterminal x, [Epsilon | ProductionRule y [] <- prules, x == y])
     | Nonterminal x <- syms]

calcFirst' cfg currTbl syms =
  let (isChanged, nextFst) = calcFirst'' cfg currTbl syms in
  if isChanged then calcFirst' cfg nextFst syms else currTbl
                                                 

calcFirst'' cfg tbl [] 
  = (False, [])
calcFirst'' cfg tbl (Terminal x:therest)
  = calcFirst''' cfg tbl (False, (Terminal x, first tbl (Terminal x))) therest
calcFirst'' cfg tbl (Nonterminal x:therest) 
  = calcFirst''' cfg tbl (ischanged, (Nonterminal x, rng)) therest
    where
      CFG start prules = cfg
      
      addendum   = f [zs | ProductionRule y zs <- prules, x == y]
      currRng    = first tbl (Nonterminal x)
      ischanged  = (\\) addendum currRng /= []
      rng        = union addendum currRng
      
      f []       = []
      f (zs:zss) = union (first_ tbl zs) (f zss)
                   
calcFirst''' cfg tbl (bool1, oneupdated) therest =
  let (bool2, therestupdated) = calcFirst'' cfg tbl therest in
  (bool1 || bool2, oneupdated:therestupdated)


--
-- follow :: [(Symbol, [ExtendedSymbol])] -> Symbol -> [ExtendedSymbol]
-- follow tbl x = case lookup x tbl of
--   Nothing -> error (show x ++ " : " ++ show tbl)
--   Just z  -> z

--
-- calcFollow :: CFG -> [(Symbol, [ExtendedSymbol])]
-- calcFollow cfg = calcFollow' (calcFirst cfg) prules (initFollow cfg) 
--   where CFG _ prules = cfg

-- initFollow cfg = 
--   let CFG start prules = cfg
--   in  [(Nonterminal x, [EndOfSymbol | x == start])
--       | Nonterminal x <- symbols cfg]
      
-- calcFollow' fstTbl prules currTbl = 
--   let (isChanged, nextFlw) = calcFollow'' fstTbl currTbl prules False in
--   if isChanged then calcFollow' fstTbl prules nextFlw else currTbl
                                                      
-- calcFollow'' fstTbl flwTbl []                            b = (b, flwTbl)
-- calcFollow'' fstTbl flwTbl (ProductionRule y zs:therest) b =
--   calcFollow'' fstTbl tbl' therest b'
--   where
--     (b',tbl') = f zs flwTbl b
    
--     _y             = Nonterminal y
    
--     f []                 tbl b = (b, tbl)
--     f [Terminal z]       tbl b = (b, tbl)
--     f [Nonterminal z]    tbl b =
--       let flwZ = follow flwTbl (Nonterminal z)
--           zRng = union flwZ (follow flwTbl _y)
--           isChanged = (\\) zRng flwZ /= []
--       in  (isChanged, upd (Nonterminal z) zRng tbl)
--     f (Terminal z:zs)    tbl b = f zs tbl b
--     f (Nonterminal z:zs) tbl b =
--       let fstZS = first_ fstTbl zs
--           flwZ  = follow flwTbl (Nonterminal z)
--           zRng  = union (follow flwTbl (Nonterminal z))
--                     (union ((\\) fstZS [Epsilon])
--                       (if elem Epsilon fstZS 
--                        then follow flwTbl _y
--                        else []))
--           isChanged = (\\) zRng flwZ /= []
--       in  f zs (upd (Nonterminal z) zRng tbl) isChanged
    
--     upd z zRng tbl = [if z == x then (x, zRng) else (x,xRng) | (x,xRng) <- tbl]
    
--     
closure :: AUGCFG -> Items -> Items
closure augCfg items = 
  if isChanged 
  then closure augCfg itemsUpdated  -- loop over items
  else items
  where
    CFG s prules = augCfg
    (isChanged, itemsUpdated) 
      = closure' (calcFirst augCfg) prules items items False
                       
                  
closure' fstTbl prules cls [] b = (b, cls)
closure' fstTbl prules cls (Item (ProductionRule x alphaBbeta) d lookahead:items) b = 
  if _Bbeta /= []
  then f cls b prules
  else closure' fstTbl prules cls items b
  where
    _Bbeta = drop d alphaBbeta
    _B     = head _Bbeta
    beta   = tail _Bbeta
    
    -- loop over production rules
    f cls b [] = closure' fstTbl prules cls items b
    f cls b (r@(ProductionRule y gamma):rs) = 
      if _B == Nonterminal y
      then (if lookahead == [] 
            then flrzero cls b r rs -- closure for LR(0)
            else g cls b r rs (extFirst_ fstTbl (map Symbol beta ++ lookahead))) -- closure for LR(1)
      else f cls b rs

    flrzero cls b r rs = 
      let item = Item r 0 []
      in  if elem item cls then f cls b rs 
          else f (cls ++ [item]) True rs

    -- loop over terminal symbols
    g cls b r rs [] = f cls b rs
    g cls b r rs (Symbol (Terminal t) : fstSyms) =
      let item = Item r 0 [Symbol (Terminal t)]
      in  if elem item cls 
          then g cls b r rs fstSyms 
          else g (cls++[item]) True r rs fstSyms
    g cls b r rs (Symbol (Nonterminal t) : fstSyms) = g cls b r rs fstSyms
    g cls b r rs (EndOfSymbol eot : fstSyms) = 
      let item = Item r 0 [EndOfSymbol eot]
      in  if elem item cls 
          then g cls b r rs fstSyms 
          else g (cls++[item]) True r rs fstSyms
    g cls b r rs (Epsilon : fstSyms) = error "closure: Epsilon"
    
--    
calcLR0Items :: AUGCFG -> Itemss
calcLR0Items augCfg = calcItems' augCfg syms iss0
  where 
    CFG _S prules = augCfg
    i0   = Item (head prules) 0 []  -- The 1st rule : S' -> S.
    is0  = closure augCfg [i0]
    iss0 = [ is0 ]

    syms = (\\) (symbols augCfg) [Nonterminal _S]
    -- syms = [ sym | sym <- symbols augCfg, sym /= Nonterminal _S]

-- calcLR1Items :: AUGCFG -> Itemss
-- calcLR1Items augCfg = calcItems' augCfg syms iss0
--   where 
--     CFG _S prules = augCfg
--     i0   = Item (head prules) 0 [EndOfSymbol]  -- The 1st rule : S' -> S.
--     is0  = closure augCfg [i0]
--     iss0 = [ is0 ]

--     syms = (\\) (symbols augCfg) [Nonterminal _S]
--     -- syms = [ sym | sym <- symbols augCfg, sym /= Nonterminal _S]
  
calcItems' augCfg syms currIss  =
  if isUpdated
  then calcItems' augCfg syms nextIss
  else currIss
  where
    (isUpdated, nextIss) = f currIss False currIss
    
    -- loop over sets of items
    f []       b currIss = (b, currIss)
    f (is:iss) b currIss = g is iss b currIss syms
    
    -- loop over symbols
    g is iss b currIss []     = f iss b currIss
    g is iss b currIss (x:xs) = 
      let is' = goto augCfg is x
      in  if is' == [] || elemItems is' currIss 
          then g is iss b currIss xs 
          else g is iss True (currIss ++ [is']) xs

elemItems :: Items -> Itemss -> Bool       
elemItems is0 []       = False
elemItems is0 (is:iss) = eqItems is0 is || elemItems is0 iss
                         
eqItems :: Items -> Items -> Bool                         
eqItems is1 is2 = (\\) is1 is2 == [] && (\\) is2 is1 == []

indexItem :: String -> Itemss -> Items -> Int
indexItem loc items item = indexItem' loc items item 0

indexItem' loc (item1:items) item2 n
  = if eqItems item1 item2 then n else indexItem' loc items item2 (n+1)
indexItem' loc [] item n = error ("indexItem: not found " ++ show item ++ " at " ++ loc)

goto :: AUGCFG -> Items -> Symbol -> Items
goto augCfg items x = closure augCfg itemsOverX
  where
    itemsOverX = [ Item (ProductionRule z alphaXbeta) (j+1) y
                 | Item (ProductionRule z alphaXbeta) j     y <- items
                 , let _Xbeta = drop j alphaXbeta
                 , _Xbeta /= []
                 , x == head _Xbeta ]
                 


--------------------------------------------------------------------------------
-- Efficient LALR Parser
--------------------------------------------------------------------------------
sharp = Terminal "#"  -- a special terminal symbol
sharpSymbol = Symbol sharp

calcEfficientLALRParseTable
  :: CFG
     -> String
     -> TokenAttrs
     -> ProdRuleAttrs
     -> IO ([Items], [ProductionRule]
           , ParserTable.ActionTable, ParserTable.GotoTable
           , ConflictsResolved)
calcEfficientLALRParseTable augCfg eot tokenAttrs prodRuleAttrs =
  do
     -- putStrLn "lr0kernelitems:"
     -- prItems stdout lr0kernelitems

     -- putStrLn "splk:"
     -- mapM_ putStrLn $ map show splk
     
     -- putStrLn "prop:"
     -- mapM_ putStrLn $ map show prop

     -- putStrLn "lr1kernelitems:"
     -- prItems stdout lr1kernelitems
     
     return (lr1items, prules, actionTable, gotoTable, conflictsResolved) 
  where
    CFG _S' prules = augCfg

    syms = (\\) (symbols augCfg) [Nonterminal _S']

    terminalSyms    = [Terminal x    | Terminal x    <- syms]
    nonterminalSyms = [Nonterminal x | Nonterminal x <- syms]

    -- | 1. Construction of LR(0) items (naively)
    lr0items = calcLR0Items augCfg
    
    -- | 2. Extract LR(0) kernel items
    lr0kernelitems = map (filter (isKernel (startNonterminal augCfg))) lr0items

    -- | 3. Spontaneous lookaheads in an item at a state
    lr0GotoTable = calcLr0GotoTable augCfg lr0items

    splk = (Item (head prules) 0 [], 0, [EndOfSymbol eot])
             : calcSplk augCfg lr0kernelitems lr0GotoTable
    
    -- | 4. Lookaheads propagation from an item at a state to another item at another state
    prop = calcProp augCfg lr0kernelitems lr0GotoTable

    -- | 5. Construction of LR(1) kernel items from splk, prop, and LR(0) kernel items
    lr1kernelitems = computeLookaheads splk prop lr0kernelitems

    -- | 6. Construction of LR(1) items
    lr1items = map (closure augCfg) lr1kernelitems

    -- | 7. Construction of LALR(1) table
    (actionTable, gotoTable, conflictsResolved) =
      calcEfficientLALRActionGotoTable augCfg eot lr1items tokenAttrs prodRuleAttrs

calcLr0GotoTable :: CFG -> [[Item]] -> [(Int, Symbol, Int)]
calcLr0GotoTable augCfg lr0items =
  nub [ (from, h, to)
      | item1 <- lr0items
      , Item (ProductionRule y ys) j lookahead <- item1
      , let from = indexItem "lr0GotoTable(from)" lr0items item1
      , let ri   = indexPrule augCfg (ProductionRule y ys)
      , let ys' = drop j ys
      , let h = head ys'
      , let to = indexItem "lr0GotoTable(to)" lr0items (goto augCfg item1 h)
      , ys' /= []
      ] 

calcSplk :: CFG -> Itemss -> [(Int, Symbol, Int)] -> [(Item, Int, [ExtendedSymbol])]
calcSplk augCfg lr0kernelitems lr0GotoTable = 
  [ (Item prule2 dot2 [], toIndex, lookahead1)
  | (fromIndex, lr0kernelitem) <- zip [0..] lr0kernelitems  -- take item for each LR(0) kernels
  , item0@(Item prule0 dot0 _) <- lr0kernelitem 
  
  , let lr1items = closure augCfg [Item prule0 dot0 [sharpSymbol]] -- Take its LR(1) closure with #
  , item1@(Item prule1@(ProductionRule lhs rhs) dot1 lookahead1) <- lr1items
  , lookahead1 /= [sharpSymbol]

  , let therestrhs = drop dot1 rhs 
  , therestrhs /= []
  , let symbolx = head therestrhs
  , let toIndexes = [t | (f,x,t) <- lr0GotoTable, f==fromIndex, x==symbolx ]
  , toIndexes /= []
  , let toIndex = head toIndexes

  , let gotoIX = lr0kernelitems !! toIndex -- for each item in GoTo(I,X)
  , item2@(Item prule2 dot2 lookahead2) <- gotoIX
  , prule1 == prule2
  ]  

calcProp :: CFG -> Itemss -> [(Int, Symbol, Int)] -> [(Item, Int, Item, Int)]
calcProp augCfg lr0kernelitems lr0GotoTable = 
  [ (Item prule0 dot0 [], fromIndex, Item prule2 dot2 [], toIndex) 
  | (fromIndex, lr0kernelitem) <- zip [0..] lr0kernelitems  -- take item for each LR(0) kernels
  , Item prule0 dot0 _ <- lr0kernelitem 
  
  , let lr1items = closure augCfg [Item prule0 dot0 [sharpSymbol]] -- Take its LR(1) closure with #
  , Item prule1@(ProductionRule lhs rhs) dot1 lookahead1 <- lr1items
  , lookahead1 == [sharpSymbol]

  , let therestrhs = drop dot1 rhs 
  , therestrhs /= []
  , let symbolx = head therestrhs
  , let toIndexes = [t | (f,x,t) <- lr0GotoTable, f==fromIndex, x==symbolx ]
  , toIndexes /= []
  , let toIndex = head toIndexes

  , let gotoIX = lr0kernelitems !! toIndex -- for each item in GoTo(I,X)
  , Item prule2 dot2 lookahead2 <- gotoIX
  , prule1 == prule2
  ]     

calcEfficientLALRActionGotoTable
  :: CFG -> String -> Itemss -> TokenAttrs -> ProdRuleAttrs
     -> (ParserTable.ActionTable, ParserTable.GotoTable, ConflictsResolved)
calcEfficientLALRActionGotoTable augCfg eot items (TokenAttrs tokenAttrs) (ProdRuleAttrs prodRuleAttrs) =
  (actionTable, gotoTable, conflictsResolved)
  where
    CFG _S' prules = augCfg
    -- items = calcLR1Items augCfg
    -- syms  = (\\) (symbols augCfg) [Nonterminal _S']
    
    -- terminalSyms    = [Terminal x    | Terminal x    <- syms]
    -- nonterminalSyms = [Nonterminal x | Nonterminal x <- syms]
    
    f :: [(ActionTable,GotoTable)] -> (ActionTable, GotoTable, ConflictsResolved)
    -- f l = case unzip l of (fst,snd) -> (g [] (concat fst), h [] (concat snd))
    f l = case unzip l of (fst,snd) ->
                            let (actTbl, conflictsResolved) = g1 (concat fst)
                                gotoTbl = h [] (concat snd)
                            in  (actTbl, gotoTbl, conflictsResolved)
                          
    g actTbl [] = actTbl
    g actTbl ((i,x,a):triples) = 
      let bs = [ (i',x',a') | (i',x',a') <- actTbl, i' == i && x' == x ] in
      if length bs == 0
      then g (actTbl ++ [(i,x,a)]) triples
      else if and [ a == a' | (_,_,a') <- bs ]
           then g actTbl triples
           else error $ "Conflict: " ++ show (i,x,a) ++ " " ++ show bs

    g1 :: ActionTable -> (ActionTable, ConflictsResolved)
    g1 actTbl = gResolve . squeeze . groupBy eqStateLookahead . sortBy cmpStateLookahead $ actTbl
      where
        squeeze xss = map (nubBy (\(_,_,a1) (_,_,a2) -> a1==a2)) xss
        
        gResolve :: [ActionTable] -> (ActionTable, ConflictsResolved)
        gResolve []           = ([], [])
        gResolve ([]:theRest) = error "gResolve: empty group" -- Will never happen
        gResolve ([(i,x,a)]:theRest)               =
          let (actTbl',conflictsResolved)  = gResolve theRest 
          in  ((i,x,a) : actTbl', conflictsResolved)
        gResolve ([t1,t2]:theRest) =    -- conflict resolution
          let (ixa, conflictResolved)      = resolve t1 t2
              (actTbl', conflictsResolved) = gResolve theRest
          in  (ixa : actTbl', conflictResolved : conflictsResolved)
        gResolve ixaList = error $ "Conflict: " ++ show (head ixaList)

        eqStateLookahead (i1,x1,a1) (i2,x2,a2) = i1==i2 && x1==x2

        cmpStateLookahead (i1,x1,a1) (i2,x2,a2) =
          if i1<i2 || i1==i2 && x1<x2 then LT
          else if i1==i2 && x1==x2 then EQ
          else GT

        -- Precondition: i1==i2 && x1==x2
        resolve t1@(i1,x1,Reduce p1) t2@(i2,x2,Reduce p2) =
          if p1 < p2
            then (t1, (i1,x1,Reduce p1, Reduce p2))
            else (t2, (i2,x2,Reduce p2, Reduce p1))
                    
        resolve t1@(i1,x1,Shift tk) t2@(i2,x2,Reduce p) =
          case (getAssocPrecToken x1, getAssocPrecProdRule p) of
            -- By the extended resolution
            (Just (assoc1, p1), Just (assoc2, p2)) ->  
              if p2 > p1                                -- if prec(rule) is higher than prec(lookahead)
              then (t2, (i2,x2,Reduce p, Shift tk))     -- then do reduce
              else if p2 == p1 && assoc2 == Attrs.Left  -- else if the same prec && assoc(rule) is Left
                      then (t2, (i2,x2,Reduce p, Shift tk))    -- then do reduce
                      else (t1, (i1,x1,Shift tk, Reduce p))    -- else do shift

            -- By the default resolution
            _ ->  
              (t1, (i1,x1,Shift tk, Reduce p))
          
        resolve t1@(i1,x1,Reduce p) t2@(i2,x2,Shift tk) = resolve t2 t1
          
        resolve t1@(i1,x1,a1) t2@(i2,x2,a2) =
          error $ "Conflict: unexpected actions: state "
                     ++ show i1 ++ show " token " ++ show x1 ++ " : "
                     ++ show a1 ++ " vs " ++ show a2

        getAssocPrecProdRule p1 =
          case [ (assoc,prec) | (p,(assoc,prec)) <- prodRuleAttrs, p==p1 ] of
            [] -> Nothing
            ((assoc,p1'):_) -> Just (assoc,p1')

        getAssocPrecToken (Symbol (Terminal s)) =
          case [ (assoc,prec) | (tok,(assoc,prec)) <- tokenAttrs, tok==s ] of
            [] -> Nothing
            ((assoc,prec):_) -> Just (assoc,prec)
        
        getAssocPrecToken (Epsilon) = Nothing
        
        getAssocPrecToken (EndOfSymbol _) = Nothing
    
    h :: GotoTable -> GotoTable -> GotoTable
    h gtTbl [] = gtTbl
    h gtTbl ((i,x,j):triples) =
      let bs = [ (i',x',j') | (i',x',j') <- gtTbl, i' == i && x' == x ] in
      if length bs == 0
      then h (gtTbl ++ [(i,x,j)]) triples
      else if and [ j' == j | (_,_,j') <- bs]
           then h gtTbl triples
           else error $ "Conflict: " ++ show (i,x,j) ++ " " ++ show bs
    
    mkLr0 (Item prule dot _) = Item prule dot []

    itemsInLr0 = map (nub . map mkLr0) items 

    (actionTable, gotoTable, conflictsResolved) = f
      [ if ys' == []
        then if y == _S' && a == EndOfSymbol eot
             then ([(from, a, Accept)   ], []) 
             else ([(from, a, Reduce ri)], [])
        else if isTerminal h 
             then ([(from, Symbol h, Shift to) ], [])
             else ([]                    , [(from, h, to)])
      | (from,item1) <- zip [0..] items -- Optimization: (from,item1) <- zip [0..] items
      , Item (ProductionRule y ys) j [a] <- item1
      -- , let from = indexItem "lr1ActionGotoTable(from)"  items item1
      , let ri   = indexPrule augCfg (ProductionRule y ys)
      , let ys' = drop j ys
      , let h = head ys'
      , let to = indexItem "lr1ActionGotoTable(to)" itemsInLr0 (goto augCfg (nub $ map mkLr0 item1) h)
      ]

type Lookahead = [ExtendedSymbol] 
type Lookaheads = [Lookahead] 
type SpontaneousLookahead = [(Item, Int, Lookahead)]
type PropagateLookahead = [(Item, Int, Item, Int)]

computeLookaheads :: SpontaneousLookahead -> PropagateLookahead -> Itemss -> Itemss
computeLookaheads splk prlk lr0kernelitemss = lr1kernelitemss
  where

    -- | initial LR(1) kernel item lookaheads
    initLr1kernelitemlkss =
      initLr1Kernel splk (zip [0..length lr0kernelitemss] lr0kernelitemss)
    
    lr1kernelitemlkss = snd (unzip (prop prlk initLr1kernelitemlkss))
    
    lr1kernelitemss = 
      [ concat [ if lookaheads == []  then [Item prule dot []]
          else [ Item prule dot lookahead | lookahead <- lookaheads ] 
          | (Item prule dot _, lookaheads) <- itemlks ]
      | itemlks <- lr1kernelitemlkss ]


-- | inintial LR(1) items
initLr1Kernel :: SpontaneousLookahead -> [(Int, Items)] -> [(Int, [(Item, Lookaheads)])]
initLr1Kernel splk [] = []
initLr1Kernel splk ((index,items):iitemss) =
  (index, init' splk index items) : initLr1Kernel splk iitemss

-- |  For each item at state index
init' :: SpontaneousLookahead -> Int -> Items -> [(Item, Lookaheads)]
init' splk index [] = []
init' splk index (item:items) = (item, init'' index item [] splk ) : init' splk index items

-- |  For each spontaneous lookahead, add it to the item when matched.
init'' :: Int -> Item -> Lookaheads -> [(Item, Int, Lookahead)] -> Lookaheads
init'' index itembase lookaheads [] = lookaheads 
init'' index itembase lookaheads ((splkitem,loc,lookahead):splkitems) = 
  if index == loc && itembase == splkitem 
  then init'' index itembase (lookaheads ++ [lookahead]) splkitems 
  else init'' index itembase lookaheads splkitems 

-- | Propagating lookaheads until no change
prop :: PropagateLookahead -> [(Int, [(Item, Lookaheads)])] -> [(Int, [(Item, Lookaheads)])]
prop prlk ilr1kernelitemlkss = 
  let itemToLks = collect ilr1kernelitemlkss prlk
      (changed, ilr1kernelitemlkss') = 
         copy ilr1kernelitemlkss itemToLks
  in  if changed then prop prlk ilr1kernelitemlkss'
      else ilr1kernelitemlkss

collect :: [(Int, [(Item, Lookaheads)])] -> PropagateLookahead -> [(Item, Int, Lookaheads)]
collect ilr1kernelitemlkss [] = []
collect ilr1kernelitemlkss (itemFromTo:itemFromTos) = 
  let (itemFrom, fromIndex, itemTo, toIndex) = itemFromTo 
      lookaheads = collect' itemFrom fromIndex [] ilr1kernelitemlkss 
  in (itemTo, toIndex, lookaheads) : collect ilr1kernelitemlkss itemFromTos

collect' :: Item -> Int -> Lookaheads -> [(Int, [(Item, Lookaheads)])] -> Lookaheads
collect' itemFrom fromIndex lookaheads [] = lookaheads
collect' itemFrom fromIndex lookaheads ((index, iitemlks):iitemlkss) = 
  if fromIndex == index 
  then collect' itemFrom fromIndex 
        (collect'' itemFrom lookaheads iitemlks) iitemlkss
  else collect' itemFrom fromIndex lookaheads iitemlkss

collect'' :: Item -> Lookaheads -> [(Item, Lookaheads)] -> Lookaheads
collect'' itemFrom lookaheads [] = lookaheads
collect'' itemFrom lookaheads ((Item prule dot _, lks):itemlks) = 
  let Item pruleFrom dotFrom _ = itemFrom
      lookaheads' = if pruleFrom == prule && dotFrom == dot
                    then accumLks lks lookaheads else lookaheads
  in collect'' itemFrom lookaheads' itemlks

-- | Eliminating space leak!
accumLks [] lookaheads = lookaheads
accumLks (lk:lks) lookaheads
  | lk `elem` lookaheads = accumLks lks lookaheads
  | otherwise = accumLks lks (lk : lookaheads)

copy :: [(Int, [(Item, Lookaheads)])] -> [(Item, Int, Lookaheads)] -> (Bool, [(Int, [(Item, Lookaheads)])])
copy iitemlkss [] = (False, iitemlkss)
copy iitemlkss (itemToLookahead:itemToLookaheads) = 
  let (changed1, iitemlkss1) = copy' iitemlkss itemToLookahead
      (changed2, iitemlkss2) = copy iitemlkss1 itemToLookaheads 
  in  (changed1 || changed2, iitemlkss2) 

copy' :: [(Int, [(Item, Lookaheads)])] -> (Item, Int, Lookaheads) -> (Bool, [(Int, [(Item, Lookaheads)])])
copy' [] itemToLookahead = (False, [])
copy' ((index,itemlks):iitemlkss) itemToLookahead = 
  let (changed1, itemlks1) = copy'' index itemlks itemToLookahead 
      (changed2, itemlkss2) = copy' iitemlkss itemToLookahead
  in  (changed1 || changed2, (index,itemlks1):itemlkss2)

copy'' :: Int -> [(Item, Lookaheads)] -> (Item, Int, Lookaheads) -> (Bool, [(Item, Lookaheads)])
copy'' index [] itemToLookahead = (False, [])
copy'' index (itemlk:itemlks) itemToLookahead = 
  let (Item prule1 dot1 _, toIndex, lookahead1) = itemToLookahead
      (Item prule2 dot2 l2, lookahead2) = itemlk  
      lookahead2' = 
        if prule1 == prule2 && dot1 == dot2 
          && index == toIndex
          && lookahead1 \\ lookahead2 /= []
          then nub (lookahead1 ++ lookahead2) else lookahead2
      changed1 = lookahead2' /= lookahead2
      itemlk1 = (Item prule2 dot2 l2, lookahead2')
      (changed2, itemlks2) = copy'' index itemlks itemToLookahead
  in (changed1 || changed2, itemlk1:itemlks2) 


prLkhTable [] = return ()
prLkhTable ((spontaneous, propagate):lkhTable) = do 
  prSpontaneous spontaneous
  prPropagate propagate
  prLkhTable lkhTable

prSpontaneous [] = return ()
prSpontaneous ((item, loc, [lookahead]):spontaneous) = do 
  putStr (show item ++ " at " ++ show loc)
  putStr ", "
  putStrLn (show lookahead)
  prSpontaneous spontaneous

prPropagate [] = return ()
prPropagate ((from, fromIndex, to, toIndex):propagate) = do 
  putStr (show from ++ " at " ++ show fromIndex)
  putStr " -prop-> "
  putStr (show to ++ " at " ++ show toIndex) 
  putStrLn ""
  prPropagate propagate

-----
-- calcLR1ParseTable :: AUGCFG -> (Itemss, ProductionRules, ActionTable, GotoTable)
-- calcLR1ParseTable augCfg = (items, prules, actionTable, gotoTable)
--   where
--     CFG _S' prules = augCfg
--     items = calcLR1Items augCfg
--     (actionTable, gotoTable) = calcLR1ActionGotoTable augCfg items 

calcLR1ActionGotoTable augCfg items = (actionTable, gotoTable)
  where
    CFG _S' prules = augCfg
    -- items = calcLR1Items augCfg
    -- syms  = (\\) (symbols augCfg) [Nonterminal _S']
    
    -- terminalSyms    = [Terminal x    | Terminal x    <- syms]
    -- nonterminalSyms = [Nonterminal x | Nonterminal x <- syms]
    
    f :: [(ActionTable,GotoTable)] -> (ActionTable, GotoTable)
    f l = case unzip l of (fst,snd) -> (g [] (concat fst), h [] (concat snd))
                          
    g actTbl [] = actTbl
    g actTbl ((i,x,a):triples) = 
      let bs = [ (i',x',a') | (i',x',a') <- actTbl, i' == i && x' == x ] in
      if length bs == 0
      then g (actTbl ++ [(i,x,a)]) triples
      else if and [ a' == a | (_,_,a') <- bs ]
           then g actTbl triples 
           else error ("Conflict: " 
                       ++ show (i,x,a) 
                       ++ " " 
                       ++ show bs)
                
    h :: GotoTable -> GotoTable -> GotoTable
    h gtTbl [] = gtTbl
    h gtTbl ((i,x,j):triples) =
      let bs = [ (i',x',j') | (i',x',j') <- gtTbl, i' == i && x' == x ] in
      if length bs == 0
      then h (gtTbl ++ [(i,x,j)]) triples
      else if and [ j' == j | (_,_,j') <- bs]
           then h gtTbl triples
           else error ("Conflict: "
                       ++ show (i,x,j)
                       ++ " "
                       ++ show bs)

    (actionTable, gotoTable) = f
      [ if ys' == []
        then if y == _S' 
             then ([(from, a, Accept)   ], []) 
             else ([(from, a, Reduce ri)], [])
        else if isTerminal h 
             then ([(from, Symbol h, Shift to) ], [])
             else ([]                    , [(from, h, to)])
      | item1 <- items -- Optimization: (from,item1) <- zip [0..] items
      , Item (ProductionRule y ys) j [a] <- item1
      , let from = indexItem "lr1ActionGotoTable(from)"  items item1
      , let ri   = indexPrule augCfg (ProductionRule y ys)  -- Can be optimzied?
      , let ys' = drop j ys
      , let h = head ys'
      , let to = indexItem "lr1ActionGotoTable(to)" items (goto augCfg item1 h)
      ]

prParseTable
  :: Handle -> (Itemss, ProductionRules, ParserTable.ActionTable,ParserTable.GotoTable) -> IO ()
prParseTable h (items, prules, actTbl, gtTbl) =
  do hPutStrLn h (show (length items) ++ " states")
     prItems h items
     hPutStrLn h ""
     prPrules h prules
     hPutStrLn h ""
     prActTbl h actTbl
     hPutStrLn h ""
     prGtTbl h gtTbl

prLALRParseTable
  :: Handle -> (Itemss, ProductionRules, [[Int]], LALRActionTable, LALRGotoTable) -> IO ()
prLALRParseTable h (items, prules, iss, lalrActTbl, lalrGtTbl) =
  do hPutStrLn h (show (length items) ++ " states")
     prItems h items
     hPutStrLn h ""
     prPrules h prules
     hPutStrLn h ""
     hPutStrLn h (show (length iss) ++ " states")
     prStates h iss
     hPutStrLn h ""
     prActTbl h lalrActTbl
     hPutStrLn h ""
     prGtTbl h lalrGtTbl
     
prStates h [] = return ()     
prStates h (is:iss) =
  do hPutStrLn h (show is)
     prStates h iss

prConflictsResolved conflictsResolved =
  do mapM_ (\(s,l,a1,a2) ->
             do putStr "Conflict resolved:"
                putStr $ " State " ++ show s
                putStr $ " on " ++ show l
                putStr $ " : " ++ show a1
                putStrLn $ " > " ++ show a2
           ) conflictsResolved

     
--------------------------------------------------------------------------------
-- LALR Parser (See an efficient one abover)
--------------------------------------------------------------------------------

-- calcLALRParseTable :: AUGCFG -> 
--                       (Itemss, ProductionRules, [[Int]], LALRActionTable
--                       , LALRGotoTable)
-- calcLALRParseTable augCfg = (itemss, prules, iss, lalrActTbl, lalrGtTbl)
--   where
--     (itemss, prules, actTbl, gtTbl) = calcLR1ParseTable augCfg
--     itemss' = nubBy eqCore itemss
--     iss     = [ [i | (i, items) <- zip [0..] itemss, eqCore items items']
--               | items' <- itemss'] 
              
--     lalrActTbl = [ (is, x, lalrAct)
--                  | is <- iss
--                  , let syms = nub [ y | i <- is, (j, y, a) <- actTbl, i == j ]
--                  , x <- syms
--                  , let lalrAct = actionCheck $
--                          nub [ toLalrAction iss a
--                              | i <- is
--                              , let r = lookupTable i x actTbl
--                              , isJust r
--                              , let Just a = r ]  ]

--     lalrGtTbl  = [ (is, x, js) 
--                  | is <- iss
--                  , let syms = nub [ y | i <- is, (j, y, k) <- gtTbl, i == j]
--                  , x <- syms
--                  , let js = stateCheck $ 
--                          nub [ toIs iss j'
--                              | i <- is
--                              , (i', x', j') <- gtTbl
--                              , i==i' && x==x' ]  ]
    
eqCore :: Items -> Items -> Bool    
eqCore items1 items2 = subsetCore items1 items2 && subsetCore items2 items1

subsetCore []             items2 = True
subsetCore (item1:items1) items2 = elemCore item1 items2 && subsetCore items1 items2
  
elemCore (Item prule1 i1 a) [] = False
elemCore (Item prule1 i1 a) (Item prule2 i2 _:items) = 
  if prule1 == prule2 && i1 == i2 
  then True else elemCore (Item prule1 i1 a) items
    
toLalrAction :: [[Int]] -> Action -> LALRAction
toLalrAction iss (Shift i)  = LALRShift (toIs iss i)
toLalrAction iss (Reduce i) = LALRReduce i
toLalrAction iss (Accept)   = LALRAccept
toLalrAction iss (Reject)   = LALRReject

toIs []       i = error ("toIs: not found" ++ show i)
toIs (is:iss) i = if elem i is then is else toIs iss i

actionCheck :: [LALRAction] -> LALRAction
actionCheck [a] = a
actionCheck as  = error ("LALR Action Conflict: " ++ show as)

stateCheck :: [[Int]] -> [Int]
stateCheck [is] = is
stateCheck iss  = error ("LALR State Conflict: " ++ show iss)
