module Verify where

import Location
import Prim
import Literal
import qualified Expr as SE
import CSType
import CSExpr


---------------------
-- Verify CS programs
---------------------

verify :: Monad m => GlobalTypeInfo -> FunctionStore -> Expr -> m ()
verify gti funStore mainexpr = do
  verifyFunStore gti funStore
  let clientFunStore = _clientstore funStore
  verifyExpr (gti,funStore) clientLoc initEnv (MonType unit_type) mainexpr

-------------------------
-- Verify function stores
-------------------------

type GlobalInfo = (GlobalTypeInfo, FunctionStore)

verifyFunStore :: Monad m => GlobalTypeInfo -> FunctionStore -> m()
  
verifyFunStore gti funStore = do
  verifyFunStoreAt gti clientLoc funStore
  verifyFunStoreAt gti serverLoc funStore

verifyFunStoreAt :: Monad m => GlobalTypeInfo -> Location -> FunctionStore -> m()
  
verifyFunStoreAt gti loc funStore =
  let gci = if loc==clientLoc then _clientstore funStore else _serverstore funStore in
  mapM_ (\(f, (codety, code)) -> verifyCode (gti,funStore) loc codety code) gci


---------------
-- Verify codes
---------------

verifyCode gtigci loc (CodeType _freeLocVars _freeTyVars freeVarTys ty)
                      (Code freeLocVars freeTyVars freeVars openCode) = do
  
  assert (_freeLocVars == freeLocVars)  --  (1) _freeLocVars==freeLocVars
    ("[verifyCode] Not equal free loc vars: "
                   ++ show _freeLocVars ++ " != " ++ show freeLocVars)
  
  assert ( _freeTyVars == freeTyVars)  --  (2) _freeTyVars==freeTyVars
    ("[verifyCode] Not equal free ty vars: "
                   ++ show _freeTyVars ++ " != " ++ show freeTyVars)
  
  assert (length freeVars == length freeVarTys)  -- (3) length freeVars==length freeVarTys
    ("[verifyCode] Not equal free variables and types: "
                   ++ show freeVars ++ " !: " ++ show freeVarTys)

  --  (4) All loc vars occurring in freeVarTys must be in freeLocVars
  --  (5) All ty vars occurring in freeVarTys must be in freeTyVars
  
  let env = Env { _locVarEnv=freeLocVars
                , _typeVarEnv=freeTyVars
                , _varEnv=zip freeVars freeVarTys}

   -- TODO: free locvars, free tyvars, free vars are closed.

  verifyOpenCode gtigci loc env ty openCode

--------------------
-- Verify open codes
--------------------

verifyOpenCode gtigci loc env (FunType argty locfun resty) (CodeAbs ((x,ty):xTys) expr) = do
  assert (null xTys)  --  (1) xTys == []
    ("[verifyOpenCode] CodeAbs has more than two args? " ++ show xTys)
  
  assert (equalType argty ty)  --   (2) argty == ty
    ("[verifyOpenCode] not equal types: " ++ show argty ++ " != " ++ show ty)

  let env1 = env {_varEnv = (x,ty) : _varEnv env}
  
  verifyExpr gtigci locfun env1 resty expr

verifyOpenCode gtigci loc env (TypeAbsType (tyvar1:tyvars1) ty) (CodeTypeAbs (tyvar2:tyvars2)  expr) = do
  --   (1) tyvar1 == tyvar2
  let _ty = if tyvar1 == tyvar2 then ty
            else doSubst [(tyvar1, TypeVarType tyvar2)] ty

  assert (tyvars1 == [])  --   (2) tyvars1 == []
    ("[verifyOpenCode] CodeTypeAbs has more than two ty args? " ++ show tyvars1)
  assert (tyvars2 == [])  --   (3) tyvars2 == []
    ("[verifyOpenCode] CodeTypeAbs has more than two ty args? " ++ show tyvars2)
  
  let env1 = env {_typeVarEnv = tyvar2 : _typeVarEnv env}

  verifyExpr gtigci loc env1 _ty expr


verifyOpenCode gtigci loc env (LocAbsType (locvar1:locvars1) ty) (CodeLocAbs (locvar2:locvars2) expr) = do
  --   (1) locvar1 == locvar2
  let _ty = if locvar1 == locvar2 then ty
            else doSubstLoc [(locvar1, LocVar locvar2)] ty

  assert (locvars1 == [])  --   (2) locvars1 == []
    ("[verifyOpenCode] CodeTypeAbs has more than two loc args? " ++ show locvars1)
  assert (locvars2 == [] ) --   (3) locvars2 == []
    ("[verifyOpenCode] CodeTypeAbs has more than two loc args? " ++ show locvars2)

  let env1 = env {_locVarEnv = locvar2 : _locVarEnv env}

  verifyExpr gtigci loc env1 _ty expr

verifyOpenCode gtigci loc env ty openCode =
  error $ "[verifyOpenCode] Not well-typed: " ++ show ty ++ "," ++ show openCode


--------------------
-- Verify code names
--------------------

verifyCodeName :: Monad m => GlobalInfo -> Location -> Type -> [Type] -> CodeName -> m ()

verifyCodeName (gti, funStore) loc someAbsTy freeVarTys (CodeName f locs tys) = 
  let locLookFor = getLoc loc someAbsTy funStore in
  let gci = if locLookFor==clientLoc then _clientstore funStore else _serverstore funStore in
        
  case [(codeType, code) | (g, (codeType, code)) <- gci, f==g] of
    [] -> error $ "[verifyCodeName] Code not found: " ++ f
    ((CodeType locvars0 tyvars0 freeVarTys0 ty, Code locvars1 tyvars1 freeVars1 _):_) -> do

      assert (locvars0 == locvars1)  --   (1) locvars0 == locvars1
        ("[verifyCodeName] No equal loc var names: "
           ++ show locvars0 ++ " != " ++ show locvars1)
      
      assert (tyvars0 == tyvars1)  --   (2) tyvars0 == tyvars1
        ("[verifyCodeName] No equal type var names: "
                       ++ show tyvars0 ++ " != " ++ show tyvars1)

      -- assert (and $ map (uncurry equalType) (zip freeVarTys0 freeVarTys))  --  (3) freeVarTys0 == freeVarTys
      --   ("[verifyCodeName] Not equal free var types: "
      --                  ++ show freeVarTys0 ++ " != " ++ show freeVarTys1)

      --  freeVarTys0 {locs/locvars0} [tys/tyvars0] == freeVarTys
      --  ty {locs/locvars0} [tys/tyvars0] == someAbsTy

      let substTy  = zip tyvars0 tys
      let substLoc = zip locvars0 locs
      
      let substed_freeVarTys0 = map (doSubstLoc substLoc . doSubst substTy) freeVarTys0
      let substed_ty = doSubstLoc substLoc (doSubst substTy ty)

      let equal (ty1, ty2) =
            assert (equalType ty1 ty2)
              ("[verifyCodeName] Not equal type: "
                 ++ show ty1 ++ " != " ++ show ty2 ++ " in " ++ f)

      equal (substed_ty, someAbsTy)
      mapM_ equal $ zip substed_freeVarTys0 freeVarTys


getLoc loc0 (FunType _ (Location loc) _) funStore = Location loc
getLoc loc0 (FunType _ (LocVar _) _) funStore = loc0
getLoc loc0 (TypeAbsType _ _) funStore = loc0
getLoc loc0 (LocAbsType _ _) funStore = loc0
getLoc loc0 ty funStore = error $ "[getLoc] unexpected type: " ++ show ty

---------------------
-- Verify expressions
---------------------

verifyExpr :: Monad m => GlobalInfo -> Location -> Env -> Type -> Expr -> m ()

verifyExpr gtigci loc env ty (ValExpr v) = verifyValue gtigci loc env ty v

verifyExpr gtigci loc env ty (Let bindingDecls expr) = do
  let (xtys, exprs) =  unzip [((x,ty), expr) | Binding x ty expr <- bindingDecls]
  let (xs, tys) = unzip xtys
  let env1 = env {_varEnv = xtys ++ _varEnv env}
  mapM_ (\ (vty, expr) -> verifyExpr gtigci loc env1 vty expr) $ zip tys exprs
  verifyExpr gtigci loc env1 ty expr

verifyExpr gtigci loc env ty (Case caseval casety alts) = do
  verifyValue gtigci loc env casety caseval
  mapM_ (verifyAlt gtigci loc env casety ty) alts 

verifyExpr gtigci loc env ty (App left (CloType (FunType argty funloc resty)) right) = do
  assert (equalLoc loc funloc)  --   (1) loc == funloc
    ("[verifyExpr] Not equal locations: " ++ show loc ++ " != " ++ show funloc)
  assert (equalType ty resty)  --   (2) ty == resty
    ("[verifyExpr] Not equal types: " ++ show ty ++ " != " ++ show resty)
  
  verifyValue gtigci loc env (CloType (FunType argty funloc resty)) left
  verifyValue gtigci loc env argty right

verifyExpr gtigci loc env ty (TypeApp left (CloType (TypeAbsType tyvars bodyty)) tys) = do
  assert (length tyvars == length tys)  --   (1) length tyvars == length tys
    ("[verifyExpr] Not equal arities: " ++ show tyvars ++ " != " ++ show tys)

  verifyValue gtigci loc env (CloType (TypeAbsType tyvars bodyty)) left
  let subst = zip tyvars tys
  let substed_bodyty = doSubst subst bodyty
  
  assert (equalType substed_bodyty ty)
    ("[verifyExpr] Not equal type: " ++ show substed_bodyty ++ " != " ++ show ty)

verifyExpr gtigci loc env ty (LocApp left (CloType (LocAbsType locvars bodyty)) locs) = do
  assert (length locvars == length locs)  --   (1) length locvars == length locs
    ("[verifyExpr] Not equal arities: " ++ show locvars ++ " != " ++ show locs)
  
  verifyValue gtigci loc env (CloType (LocAbsType locvars bodyty)) left
  let substLoc = zip locvars locs
  let substed_bodyty = doSubstLoc substLoc bodyty
  
  assert (equalType substed_bodyty ty)
    ("[verifyExpr] Not equal type: " ++ show substed_bodyty ++ " != " ++ show ty)

verifyExpr gtigci loc env ty (Prim MkRecOp locs tys vs) = do -- locs=[], tys=[]
  return ()
  
verifyExpr gtigci loc env ty (Prim prim op_locs op_tys vs) = do
  case lookupPrimOpType prim of
    [] -> error $ "[verifyExpr] Not found prim: " ++ show prim
    ((locvars, tyvars, argtys,resty):_) -> do
       let substTy  = zip tyvars op_tys
       let substLoc = zip locvars op_locs
       let substed_argtys = map (doSubstLoc substLoc . doSubst substTy) argtys

       assert (length vs==length argtys
               && length locvars==length op_locs
               && length tyvars==length op_tys)
              ("[verifyExpr] unexpected: "
                 ++ show prim ++ " " ++ show op_locs ++ " " ++ show op_tys ++ " " ++  show vs
                 ++ "\n   " ++ show locvars ++ " " ++ show tyvars)

       mapM_ (\ (argty, v) -> verifyValue gtigci loc env argty v) (zip argtys vs)
       assert (equalType ty resty)  --   (1) ty == resty
          ("[verifyExpr] Not equal types: " ++ show ty ++ " != " ++ show resty)
       
verifyExpr gtigci loc env ty expr = 
  error $ "[verifyExpr]: not well-typed: " ++ show expr ++ " : " ++ show ty


verifyAlt :: Monad m => GlobalInfo -> Location -> Env -> Type -> Type -> Alternative -> m ()

verifyAlt gtigci loc env (ConType tyconname locs tys) retty (Alternative cname args expr) =
  case lookupConstr (fst gtigci) cname of
    ((bare_argtys, tyconname1, locvars, tyvars):_) -> do
      assert (tyconname==tyconname1)
        ("[verifyAlt] Not equal type con name: "
          ++ tyconname ++ " != " ++ tyconname1 ++ " for " ++ cname)
      assert (length bare_argtys==length args)
        ("[verifyAlt] Not equal arg length: "
          ++ tyconname ++ " != " ++ tyconname1 ++ " for " ++ cname)
      let substLoc = zip locvars locs
      let substTy  = zip tyvars  tys
      let argstys = map (doSubst substTy . doSubstLoc substLoc) bare_argtys
      let env1 = env {_varEnv = zip args argstys ++ _varEnv env}
      verifyExpr gtigci loc env1 retty expr
      
    [] -> error $ "[verifyAlt] Constructor not found " ++ cname

verifyAlt gtigci loc env (TupleType argtys) retty (TupleAlternative args expr) = do
  let env1 = env {_varEnv = zip args argtys ++ _varEnv env}
  verifyExpr gtigci loc env1 retty expr

----------------
-- Verify values
----------------

verifyValue :: Monad m => GlobalInfo -> Location -> Env -> Type -> Value -> m ()

verifyValue gtigci loc env ty (Var x) = do
  case [ty | (y,ty) <- _varEnv env, x==y] of
    (yty:_) -> assert (equalType yty ty)
                  ("[verifyValue] Not equal type: " ++ show yty ++ " != " ++ show ty)
    []    ->
      case [ty | (z,ty) <- _libInfo $ fst $ gtigci, x==z] of
        (zty:_) -> assert (equalType zty ty)
                     ("[verifyValue] Not equal type: " ++ show zty ++ " != " ++ show ty)
        [] -> error $ "[verifyExpr] Variable not found: " ++ x ++ " in " ++ show (_varEnv env)

verifyValue gtigci loc env ty (Lit lit) =
  case lit of
    IntLit i  -> assert (equalType ty int_type) "[verifyValue] Not Int type"
    StrLit s  -> assert (equalType ty string_type) "[verifyValue] Not String type"
    BoolLit b -> assert (equalType ty bool_type) "[verifyValue] Not Bool type"
    UnitLit   -> assert (equalType ty unit_type) "[verifyValue] Not Unit type"

verifyValue gtigci loc env (TupleType tys) (Tuple vs) =
  mapM_ ( \ (ty,v) -> verifyValue gtigci loc env ty v ) (zip tys vs)

verifyValue gtigci loc env ty (Constr cname locs tys args argtys) = do
  mapM_ ( \ (ty,v) -> verifyValue gtigci loc env ty v ) (zip argtys args)
  case lookupConstr (fst gtigci) cname of
    ((bare_argtys, tyconname, locvars, tyvars):_) -> do
      let substLoc = zip locvars locs
      let substTy  = zip tyvars tys
      let argtys1 = map (doSubst substTy . doSubstLoc substLoc) bare_argtys
      assert (and (map (uncurry equalType) (zip argtys1 argtys)))  -- argstys1 == argtys
        ("[verifyValue] Not equal constructor arg types: " ++ cname ++ " "
           ++ show argtys1 ++ " != " ++ show argtys)
      assert (equalType (ConType tyconname locs tys) ty)  -- ConType tyconname locs tys == ty
        ("[verifyValue] Not equal constructor type: " ++ cname 
           ++ show ty ++ " != " ++ show (ConType tyconname locs tys))
    [] -> error $ "[verifyValue] Constructor not found: " ++ cname

verifyValue gtigci loc env (CloType ty) (Closure vs tys codeName recf) = do
  -- let env0 = env {_varEnv = [] }
  mapM_ ( \ (ty,v) -> verifyValue gtigci loc env ty v) (zip tys vs)
  verifyCodeName gtigci loc ty tys codeName

verifyValue gtigci loc env (MonType ty) (UnitM v) = verifyValue gtigci loc env ty v

verifyValue gtigci loc env (MonType ty) (BindM bindingDecls expr) = do
  let (xtys, exprs) =  unzip [((x,ty), expr) | Binding x ty expr <- bindingDecls]
  let (xs, tys) = unzip xtys
  let env1 = env {_varEnv = xtys ++ _varEnv env}
  let monadic_tys = map MonType tys
  mapM_ (\ (mty, expr) -> verifyExpr gtigci loc env1 mty expr) $ zip monadic_tys exprs
  verifyExpr gtigci loc env1 (MonType ty) expr
  
verifyValue gtigci loc env ty (Req left (CloType (FunType argty funloc resty)) right) = do
  assert (equalLoc loc clientLoc)  --   (1) loc == client
    ("[verifyValue] Not client location: " ++ show loc)
  assert (equalLoc funloc serverLoc)  --   (2) funloc == server
    ("[verifyValue] Not server location: " ++ show funloc)
  assert (equalType ty resty)  --   (3) ty == resty
    ("[verifyExpr] Not equal types: " ++ show ty ++ " != " ++ show resty)
  
  verifyValue gtigci loc env (CloType (FunType argty funloc resty)) left
  verifyValue gtigci loc env argty right

verifyValue gtigci loc env ty (Call left (CloType (FunType argty funloc resty)) right) = do
  assert (equalLoc loc serverLoc)  --   (1) loc == server
    ("[verifyValue] Not server location: " ++ show loc)
  assert (equalLoc funloc clientLoc)  --   (2) funloc == client
    ("[verifyValue] Not client location: " ++ show funloc)
  assert (equalType ty resty)  --   (3) ty == resty
    ("[verifyValue] Not equal types: " ++ show ty ++ " != " ++ show resty)
  
  verifyValue gtigci loc env (CloType (FunType argty funloc resty)) left
  verifyValue gtigci loc env argty right

verifyValue gtigci loc env ty (GenApp funloc0 left (CloType (FunType argty funloc resty)) right) = do
  assert (equalType ty resty)  --   (1) ty == resty
    ("[verifyValue] Not equal types: " ++ show ty ++ " != " ++ show resty)
  assert (equalLoc funloc0 funloc)  --   (2) funloc0 == funloc
    ("[verifyValue] Not equal locations: " ++ show funloc0 ++ " != " ++ show funloc)
  
  verifyValue gtigci loc env (CloType (FunType argty funloc resty)) left
  verifyValue gtigci loc env argty right

verifyValue gtigci loc env ty value =
  error $ "[verifyValue]: not well-typed: " ++ show value ++ " : " ++ show ty

---
assert cond msg = if cond then return () else error msg
