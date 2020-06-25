module BasicLib where

import Location
import Type
import Prim
import Expr


basicLib :: [(String, Type, Expr)]
basicLib =
  [

--   read : {l}. Unit -l-> String
--         = {l}. \x:Unit @l. primRead [l] x
     let l = "l"
         x = "x"
     in 
     ( "read"
     , LocAbsType [l]
          (FunType unit_type (LocVar l) string_type)
     , LocAbs [l]
          (Abs [(x,unit_type,LocVar l)]
               (Prim PrimReadOp [LocVar l] [] [Var x]))
     ),

    
--   print : {l}. String -l->unit
--         = {l}. \x:String @l. primPrint [l] x

     let l = "l"
         x = "x"
     in 
     ( "print"
     , LocAbsType [l]
          (FunType string_type (LocVar l) unit_type)
     , LocAbs [l]
          (Abs [(x,string_type,LocVar l)]
               (Prim PrimPrintOp [LocVar l] [] [Var x]))
     ),
    

--   intToString
--     : {l}. Int -l-> String
--     = {l}. \x:Int @l. primIntToString [l] x
      let l = "l"
          x = "x"
      in
      ( "intToString"
      , LocAbsType [l]
          (FunType int_type (LocVar l) string_type)
      , LocAbs [l]
          (Abs [(x,int_type,LocVar l)]
            (Prim PrimIntToStringOp [LocVar l] [] [Var x]))
      ),

--   concat
--     : {l}. String -l-> String -l-> String
--     = {l}. \x:String @l  y:String @l. primConcat {l} (x,y)

      let l = "l"
          x = "x"
          y = "y"
      in
      ( "concat"
      , LocAbsType [l]
           (FunType string_type (LocVar l)
              (FunType string_type (LocVar l) string_type))
      , LocAbs [l]
           (Abs [(x,string_type,LocVar l)]
             (Abs [(y,string_type,LocVar l)]
                 (Prim PrimConcatOp [LocVar l] [] [Var x, Var y])))
      ),
    
  -- ("not", let l = "l" in
  --     LocAbsType [l] (FunType bool_type (LocVar l) bool_type)),


--    ref : {l1}. [a]. a -l1-> Ref {l1} [a]
--        = {l1}. [a].
--          \v : a @ l1. primRef {l1} [a] v

      let l1 = "l1"
          a  = "a"
          tyvar_a = TypeVarType a
          x  = "x"
      in
      ("ref"
      , LocAbsType [l1]
           (TypeAbsType [a]
               (FunType tyvar_a (LocVar l1)
                (ConType refType [LocVar l1] [tyvar_a])))
      , LocAbs [l1]
             (TypeAbs [a]
                 (Abs [(x,tyvar_a,LocVar l1)]
                    (Prim PrimRefCreateOp [LocVar l1] [tyvar_a] [Var x])))
      ),


--   (!) : {l1}. [a]. Ref {l1} [a] -l1-> a
--       = {l1}. [a].
--         \addr:Ref {l1} [a] @l1. primRefRead {l1} [a] addr

     let l1 = "l1" 
         a  = "a"
         tyvar_a = TypeVarType a
         x  = "x"
     in
     ( "!"
     , LocAbsType [l1]
          (TypeAbsType [a]
             (FunType (ConType refType [LocVar l1] [tyvar_a])
                 (LocVar l1) tyvar_a))
     , LocAbs [l1]
          (TypeAbs [a]
             (Abs [(x,ConType refType [LocVar l1] [tyvar_a],LocVar l1)]
                 (Prim PrimRefReadOp [LocVar l1] [tyvar_a] [Var x])))
     ),


--  (:=) : {l1}. [a]. Ref {l1} [a] -l1-> a -l1-> Unit
--       = {l1}. [a].
--         \addr: Ref {l1} [a] @l1. newv: a @l1. primWrite {l1} [a] addr newv


     let l1 = "l1" 
         a  = "a"
         tyvar_a = TypeVarType a
         x  = "x"
         y  = "y"
     in
     ( ":="
     , LocAbsType [l1]
          (TypeAbsType [a]
              (FunType
                   (ConType refType [LocVar l1] [tyvar_a])
                   (LocVar l1)
                   (FunType tyvar_a (LocVar l1) unit_type)))
     , LocAbs [l1]
          (TypeAbs [a]
              (Abs [(x,ConType refType [LocVar l1] [tyvar_a],LocVar l1)]
                   (Abs [(y,tyvar_a,LocVar l1)]
                       (Prim PrimRefWriteOp [LocVar l1] [tyvar_a] [Var x, Var y]))))
     )
  ]
