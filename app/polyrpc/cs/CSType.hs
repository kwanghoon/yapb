{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module CSType where

import Text.JSON.Generic

import Location
import Prim

data Type =
    TypeVarType String
  | TupleType [Type]
  | FunType Type Location Type
  | TypeAbsType [String] Type
  | LocAbsType [String] Type
  | ConType String [Location] [Type]
  | CloType Type   -- Clo A
  | MonType Type   -- T A
  deriving (Show, Typeable, Data)

data CodeType =
    CodeType [String] [String] [Type] Type  -- [alpha] [loc]. [type]. Type
    deriving (Show, Typeable, Data)

--
doSubstOne :: String -> Type -> Type -> Type

doSubstOne x ty (TypeVarType y)
  | x==y = ty
  | otherwise = (TypeVarType y)
doSubstOne x ty (TupleType tys) = TupleType (map (doSubstOne x ty) tys)
doSubstOne x ty (FunType argty loc retty) =
  FunType (doSubstOne x ty argty) loc (doSubstOne x ty retty)
doSubstOne x ty (TypeAbsType tyvars bodyty)
  | elem x tyvars = (TypeAbsType tyvars bodyty)
  | otherwise = (TypeAbsType tyvars (doSubstOne x ty bodyty))
doSubstOne x ty (LocAbsType locvars bodyty) =
  LocAbsType locvars (doSubstOne x ty bodyty)
doSubstOne x ty (ConType name locs tys) =
  ConType name locs (map (doSubstOne x ty) tys)
doSubstOne x ty (CloType innerty) =  CloType (doSubstOne x ty innerty)
doSubstOne x ty (MonType valty) = MonType (doSubstOne x ty valty)

doSubst :: [(String,Type)] -> Type -> Type
doSubst [] ty0 = ty0
doSubst ((x,ty):subst) ty0 = 
  doSubst subst (doSubstOne x ty ty0)

--
doSubstLocOne :: String -> Location -> Type -> Type

doSubstLocOne x loc (TypeVarType y) = (TypeVarType y)
doSubstLocOne x loc (TupleType tys) = TupleType (map (doSubstLocOne x loc) tys)
doSubstLocOne x loc (FunType argty loc0 retty) =
  FunType (doSubstLocOne x loc argty)
    (doSubstLocOverLoc x loc loc0) (doSubstLocOne x loc retty)
doSubstLocOne x loc (TypeAbsType tyvars bodyty) =
  TypeAbsType tyvars (doSubstLocOne x loc bodyty)
doSubstLocOne x loc (LocAbsType locvars bodyty)
  | elem x locvars = LocAbsType locvars bodyty
  | otherwise = LocAbsType locvars (doSubstLocOne x loc bodyty)
doSubstLocOne x loc (ConType name locs tys) =
  ConType name (map (doSubstLocOverLoc x loc) locs) (map (doSubstLocOne x loc) tys)
doSubstLocOne x loc (CloType innerTy) = CloType (doSubstLocOne x loc innerTy)
doSubstLocOne x loc (MonType valTy) = MonType (doSubstLocOne x loc valTy)

doSubstLoc :: [(String, Location)] -> Type -> Type
doSubstLoc [] ty = ty
doSubstLoc ((x,loc):substLoc) ty =
  doSubstLoc substLoc (doSubstLocOne x loc ty)

--
equalType :: Type -> Type -> Bool
equalType ty1 ty2 = equalTypeWithFreshness [1..] ty1 ty2

equalTypeWithFreshness ns (TypeVarType x) (TypeVarType y) = x==y

equalTypeWithFreshness ns (TupleType tys1) (TupleType tys2) =
  and (map (uncurry (equalTypeWithFreshness ns)) (zip tys1 tys2))
  
equalTypeWithFreshness ns (FunType argty1 loc1 retty1) (FunType argty2 loc2 retty2) =
  equalTypeWithFreshness ns argty1 argty2 && equalLoc loc1 loc2 && equalTypeWithFreshness ns retty1 retty2
  
equalTypeWithFreshness ns (TypeAbsType tyvars1 ty1) (TypeAbsType tyvars2 ty2) =
  let len1 = length tyvars1
      len2 = length tyvars2
      newvars = map (TypeVarType . show) (take len1 ns)
      ns'     = drop len1 ns
  in len1==len2 && equalTypeWithFreshness ns' (doSubst (zip tyvars1 newvars) ty1) (doSubst (zip tyvars2 newvars) ty2)
     
equalTypeWithFreshness ns (LocAbsType locvars1 ty1) (LocAbsType locvars2 ty2) =
  let len1 = length locvars1
      len2 = length locvars2
      newvars = map (LocVar . show) (take len1 ns)
      ns'     = drop len1 ns
  in len1==len2 && equalTypeWithFreshness ns' (doSubstLoc (zip locvars1 newvars) ty1) (doSubstLoc (zip locvars2 newvars) ty2)

equalTypeWithFreshness ns (ConType name1 locs1 tys1) (ConType name2 locs2 tys2) =   
  name1==name2 && equalLocs locs1 locs2 && and (map (uncurry (equalTypeWithFreshness ns)) (zip tys1 tys2))

equalTypeWithFreshness ns (CloType innerTy1) (CloType innerTy2) =
  equalTypeWithFreshness ns innerTy1 innerTy2

equalTypeWithFreshness ns (MonType valTy1) (MonType valTy2) =
  equalTypeWithFreshness ns valTy1 valTy2

equalTypeWithFreshness ns ty1 ty2 = False

--
primType tyname = ConType tyname [] []

bool_type = primType boolType
int_type  = primType intType
unit_type = primType unitType
string_type = primType stringType
