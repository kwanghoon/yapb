{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module CSExpr where

import qualified Data.Set as Set

import Location
import Prim
import Literal
import CSType
import qualified Expr as SE
import Text.JSON.Generic

data Expr =
    ValExpr Value
  | Let [BindingDecl] Expr
  | Case Value Type [Alternative]  -- including pi_i (V)
  | App Value Type Value
  | TypeApp Value Type [Type]
  | LocApp Value Type [Location]
  | Prim PrimOp [Location] [Type] [Value]
  deriving (Show, Typeable, Data)

data Value =
    Var String
  | Lit Literal
  | Tuple [Value]
  | Constr String [Location] [Type] [Value] [Type]
  | Closure [Value] [Type] CodeName  [String] -- [] or [rec_f] for now, [rec_f1, ...,, rec_fk] in future
  | UnitM Value
  | BindM [BindingDecl] Expr
  | Req Value Type Value
  | Call Value Type Value
  | GenApp Location Value Type Value

  -- Runtime values
  | Addr Integer  
  deriving (Show, Typeable, Data)

data BindingDecl =
    Binding String Type Expr
    deriving (Show, Typeable, Data)

data DataTypeDecl =
    DataType String [String] [TypeConDecl]
-- For aeson  
--  deriving (Show, Generic)
    deriving (Show, Typeable, Data)

data TopLevelDecl =
    BindingTopLevel BindingDecl
  | DataTypeTopLevel DataTypeDecl
  | LibDeclTopLevel String Type 
-- For aeson  
--  deriving (Show, Generic)
    deriving (Show, Typeable, Data)

data TypeConDecl =
   TypeCon String [Type]
-- For aeson  
--  deriving (Show, Generic)
    deriving (Show, Typeable, Data)
    
data Alternative =
    Alternative String [String] Expr
  | TupleAlternative [String] Expr    
  deriving (Show, Typeable, Data)

data Code =
    Code [String] [String] [String] OpenCode  -- [loc]. [alpha]. [x]. OpenCode
    deriving (Show, Typeable, Data)

data OpenCode =
    CodeAbs     [(String, Type)] Expr
  | CodeTypeAbs [String] Expr
  | CodeLocAbs  [String] Expr
  deriving (Show, Typeable, Data)
  

data CodeName =
    CodeName String [Location] [Type] 
    deriving (Show, Typeable, Data)

--
-- [(Name, Location Vars, Type Vars)]
type TypeInfo = [(String, [String], [String])] 

-- [(ConName, (ConArgTypes, DTName, LocationVars, TypeVars))]
type ConTypeInfo = [(String, ([Type], String, [String], [String]))] 

type BindingTypeInfo = [(String, Type)]

-- [ (DTName, LocationVars, TypeVars, [(ConName, ArgTypes)]) ]
type DataTypeInfo = [(String, ([String], [String], [(String,[Type])]))]

type LibInfo = [(String, Type)]

data GlobalTypeInfo = GlobalTypeInfo
   { _typeInfo :: TypeInfo
   , _conTypeInfo :: ConTypeInfo
   , _dataTypeInfo :: DataTypeInfo
   , _libInfo :: LibInfo } -- library types
    deriving (Show, Typeable, Data)
       
data Env = Env
   { _locVarEnv  :: [String]
   , _typeVarEnv :: [String]
   , _varEnv     :: BindingTypeInfo }

initEnv = Env { _locVarEnv=[], _typeVarEnv=[], _varEnv=[] }

--
data FunctionStore = FunctionStore
   { _clientstore :: [(String, (CodeType, Code))]
   , _serverstore :: [(String, (CodeType, Code))]
   , _new   :: Int
   }
   deriving (Show, Typeable, Data)

addClientFun :: FunctionStore -> String -> CodeType -> Code -> FunctionStore
addClientFun fnstore name ty code =
   fnstore {_clientstore = _clientstore fnstore ++ [(name,(ty,code))] }

addServerFun :: FunctionStore -> String -> CodeType -> Code -> FunctionStore
addServerFun fnstore name ty code =
   fnstore {_serverstore = (_serverstore fnstore) ++ [(name,(ty,code))] }

addFun :: Location -> FunctionStore -> String -> CodeType -> Code -> FunctionStore
addFun loc funstore name ty@(CodeType [] [] fvtys (FunType _ funloc _)) code =
  if isClient funloc then addClientFun funstore name ty code
  else if isServer funloc then addServerFun funstore name ty code
  else addServerFun (addClientFun funstore name ty code) name ty code
addFun loc funstore name ty@(CodeType [] [] fvtys somety) code =
  addServerFun (addClientFun funstore name ty code) name ty code
addFun loc funstore name ty@(CodeType locvars tyvars fvtys somety) code =
  addServerFun (addClientFun funstore name ty code) name ty code

newName :: FunctionStore -> (String, FunctionStore)
newName fnstore = let n = _new fnstore in ("f" ++ show n, fnstore{_new =n+1})

newVar :: FunctionStore -> (String, FunctionStore)
newVar fnstore = let n = _new fnstore in ("x" ++ show n, fnstore{_new =n+1})

newVars :: Int -> FunctionStore -> ([String], FunctionStore)
newVars 0 funStore = ([], funStore)
newVars n funStore = 
    let (x,  funStore1) = newVar funStore
        (xs, funStore2) = newVars (n-1) funStore1
    in  (x:xs, funStore2)

initFunctionStore = FunctionStore
   { _clientstore=[]
   , _serverstore=[]
   , _new        = 1
   }
   
--
--
primOpTypes :: [(PrimOp, ([String], [String], [Type], Type))] -- (locvars, tyvars, argtys, retty)
primOpTypes =
  [ (NotPrimOp, (["l"], [], [bool_type], bool_type))
  , (OrPrimOp,  (["l"], [], [bool_type, bool_type], bool_type))
  , (AndPrimOp, (["l"], [], [bool_type, bool_type], bool_type))
  , (EqPrimOp,  (["l"], [], [bool_type, bool_type], bool_type))
  , (NeqPrimOp, (["l"], [], [bool_type, bool_type], bool_type))
  , (LtPrimOp,  (["l"], [], [int_type, int_type], bool_type))
  , (LePrimOp,  (["l"], [], [int_type, int_type], bool_type))
  , (GtPrimOp,  (["l"], [], [int_type, int_type], bool_type))
  , (GePrimOp,  (["l"], [], [int_type, int_type], bool_type))
  , (AddPrimOp, (["l"], [], [int_type, int_type], int_type))
  , (SubPrimOp, (["l"], [], [int_type, int_type], int_type))
  , (MulPrimOp, (["l"], [], [int_type, int_type], int_type))
  , (DivPrimOp, (["l"], [], [int_type, int_type], int_type))
  , (NegPrimOp, (["l"], [], [int_type], int_type))

  , (PrimReadOp, (["l"], [], [unit_type], string_type))
  , (PrimPrintOp, (["l"], [], [string_type], unit_type))
  , (PrimIntToStringOp, (["l"], [], [int_type], string_type))
  , (PrimConcatOp, (["l"], [], [string_type,string_type], string_type))

  , (PrimRefCreateOp,
      let l1 = "l1" in
      let a  = "a"  in                 
      let tyvar_a = TypeVarType a in
      let locvar_l1 = LocVar l1 in
        ([l1], [a], [tyvar_a], ConType refType [locvar_l1] [tyvar_a]))
    
  , (PrimRefReadOp,
      let l1 = "l1" in
      let a  = "a"  in
      let tyvar_a = TypeVarType a in
      let locvar_l1 = LocVar l1 in
        ([l1], [a], [ConType refType [locvar_l1] [tyvar_a]], tyvar_a))
    
  , (PrimRefWriteOp,
     let l1 = "l1" in
     let a  = "a"  in
     let tyvar_a = TypeVarType a in
     let locvar_l1 = LocVar l1 in
        ([l1], [a], [ConType refType [locvar_l1] [tyvar_a], tyvar_a], unit_type))

  ]

lookupPrimOpType primop =
  [ (locvars,tyvars,tys,ty)
  | (primop1,(locvars, tyvars, tys,ty)) <- primOpTypes, primop==primop1]

lookupConstr :: GlobalTypeInfo -> String -> [([Type], String, [String], [String])]
lookupConstr gti x = [z | (con, z) <- _conTypeInfo gti, x==con]

-----------------
-- free variables
-----------------

fvOpenCode :: OpenCode -> Set.Set String

fvOpenCode (CodeAbs xTys expr) = fvExpr expr `Set.difference` Set.fromList (map fst xTys)
fvOpenCode (CodeTypeAbs tyvars expr) = fvExpr expr
fvOpenCode (CodeLocAbs locvars expr) = fvExpr expr


fvExpr :: Expr -> Set.Set String

fvExpr (ValExpr val) = fvValue val
fvExpr (Let bindingDcl expr) = Set.empty
fvExpr (Case val _ alts) = fvValue val `Set.union` Set.unions (map fvAlt alts)
fvExpr (App left _ right) = fvValue left `Set.union` fvValue right
fvExpr (TypeApp left _ _) = fvValue left
fvExpr (LocApp left _ _) = fvValue left
fvExpr (Prim primop locs tys vs) = Set.unions (map fvValue vs)


fvAlt :: Alternative -> Set.Set String

fvAlt (Alternative cname xs expr) = fvExpr expr `Set.difference` Set.fromList xs
fvAlt (TupleAlternative xs expr) = fvExpr expr `Set.difference` Set.fromList xs


fvValue :: Value -> Set.Set String

fvValue (Var x) = Set.singleton x
fvValue (Lit lit) = Set.empty
fvValue (Tuple vs) = Set.unions (map fvValue vs)
fvValue (Constr cname _ _ vs _) = Set.unions (map fvValue vs)
fvValue (Closure vs _ codename _) = Set.unions (map fvValue vs)
fvValue (UnitM v) = fvValue v
fvValue (BindM bindingDecls expr) =
  (Set.unions (map (\(Binding _ _ expr) -> fvExpr expr) bindingDecls) `Set.union` fvExpr expr)
  `Set.difference` (Set.fromList (map (\(Binding x _ _) -> x) bindingDecls))
fvValue (Req left _ right) = fvValue left `Set.union` fvValue right
fvValue (Call left _ right) = fvValue left `Set.union` fvValue right
fvValue (GenApp _ left _ right) = fvValue left `Set.union` fvValue right


--
singleBindM (BindM [] expr) = expr
singleBindM (BindM (bind:binds) expr) =
  ValExpr $ BindM [bind] (singleBindM (BindM binds expr))
