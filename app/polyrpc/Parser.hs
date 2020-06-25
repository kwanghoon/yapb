module Parser where

import CommonParserUtil
import Location
import Token
import Type
import Prim
import Literal
import Expr


parserSpec :: ParserSpec Token AST
parserSpec = ParserSpec
  {
    startSymbol = "TopLevel'",
    
    parserSpecList =
    [
      ("TopLevel' -> TopLevel", \rhs -> get rhs 1),

      {- Identifiers -}
      ("Identifiers -> identifier", \rhs -> toASTIdSeq [getText rhs 1] ),

      ("Identifiers -> identifier Identifiers",
        \rhs -> toASTIdSeq (getText rhs 1 : fromASTIdSeq (get rhs 2)) ),


      {- OptIdentifiers -}
      ("OptIdentifiers -> ", \rhs -> toASTIdSeq [] ),

      ("OptIdentifiers -> Identifiers", \rhs -> get rhs 1 ),


      {- IdentifierCommas -}
      ("IdentifierCommas -> identifier", \rhs -> toASTIdSeq [getText rhs 1] ),

      ("IdentifierCommas -> identifier , IdentifierCommas",
        \rhs -> toASTIdSeq (getText rhs 1 : fromASTIdSeq (get rhs 3)) ),


      {- OptIdentifierCommas -}
      ("OptIdentifierCommas -> ", \rhs -> toASTIdSeq [] ),

      ("OptIdentifierCommas -> IdentifierCommas", \rhs -> get rhs 1 ),


      {- Location -}
      ("Location -> identifier", \rhs -> toASTLocation (Location (getText rhs 1)) ),


      {- Locations -}
      ("Locations -> Identifiers", \rhs ->
        toASTLocationSeq (map Location (fromASTIdSeq (get rhs 1))) ),


      {- Type -}
      ("Type -> LocFunType", \rhs -> get rhs 1 ),

      ("Type -> { Identifiers } . Type", \rhs ->
        toASTType (singleLocAbsType
                            (LocAbsType (fromASTIdSeq (get rhs 2))
                                        (fromASTType (get rhs 5)))) ),

      ("Type -> [ Identifiers ] . Type", \rhs ->
        toASTType (singleTypeAbsType (TypeAbsType
                                              (fromASTIdSeq (get rhs 2))
                                              (fromASTType (get rhs 5)))) ),


      {- LocFunType -}
      ("LocFunType -> AppType", \rhs -> get rhs 1),
      
      ("LocFunType -> AppType LocFun LocFunType", \rhs ->
          let locfun = getText rhs 2
              loc = init (init (tail locfun))  -- extract Loc from -Loc-> ( a bit hard-coded!!)
          in  toASTType (FunType
                          (fromASTType (get rhs 1))
                          (Location loc)
                          (fromASTType (get rhs 3))) ),


      {- AppType -}
      ("AppType -> AtomicType", \rhs -> get rhs 1),

      ("AppType -> AppType { Locations }", \rhs ->
          let locs = fromASTLocationSeq (get rhs 3) in
          case fromASTType (get rhs 1) of
            ConType name [] [] -> toASTType (ConType name locs [])
            ConType name [] tys -> 
              error $ "[Parser] Not supported: types and then locations: " ++ show locs ++ " " ++ show tys
            ConType name locs' tys ->
              error $ "[Parser] Not supported: multiple locations" ++ name ++ " " ++ show locs' ++ " " ++ show locs
            TypeVarType name -> toASTType (ConType name locs [])
            ty ->
              error $ "[Parser] Not supported yet: " ++ show ty ++ " not ConType: " ++ show locs),

      ("AppType -> AppType [ LocFunTypes ]", \rhs ->
          let tys = fromASTTypeSeq (get rhs 3) in
          case fromASTType (get rhs 1) of
            ConType name locs [] -> toASTType (ConType name locs tys)
            ConType name locs tys' ->
              error $ "[Parser] Not supported: multiple types: " ++ name ++ " " ++ show tys' ++ " " ++ show tys
            TypeVarType name -> toASTType (ConType name [] tys)
            ty ->
              error $ "[Parser] Not supported yet: " ++ show ty ++ " not ConType: " ++ show tys),


      {- AtomicType -}
      ("AtomicType -> TupleType", \rhs -> get rhs 1 ),

      ("AtomicType -> ( Type )", \rhs -> get rhs 2 ),

      ("AtomicType -> identifier", \rhs -> toASTType (TypeVarType (getText rhs 1)) ),
      

      {- TupleType -}
      ("TupleType -> ( Type , TypeSeq )",
        \rhs -> toASTType (TupleType $
            (fromASTType (get rhs 2)) : (fromASTTypeSeq (get rhs 4))) ),


      {- TypeSeq -}
      ("TypeSeq -> Type", \rhs -> toASTTypeSeq [fromASTType (get rhs 1)] ),

      ("TypeSeq -> Type , TypeSeq",
        \rhs -> toASTTypeSeq $ fromASTType (get rhs 1) : (fromASTTypeSeq (get rhs 3)) ),


      {- LocFunTypes -}
      ("LocFunTypes -> LocFunType", \rhs -> toASTTypeSeq [fromASTType (get rhs 1)] ),

      ("LocFunTypes -> LocFunType LocFunTypes",
        \rhs -> toASTTypeSeq $ fromASTType (get rhs 1) : fromASTTypeSeq (get rhs 2) ),


      {- OptLocFunTypes -}
      ("OptLocFunTypes -> ", \rhs -> toASTTypeSeq [] ),

      ("OptLocFunTypes -> LocFunTypes", \rhs -> get rhs 1 ),


      {- TopLevel -}
      ("TopLevel -> Binding",
        \rhs -> toASTTopLevelDeclSeq [BindingTopLevel (fromASTBindingDecl (get rhs 1 ))] ),

      ("TopLevel -> Binding ; TopLevel",
        \rhs -> toASTTopLevelDeclSeq
            $ BindingTopLevel (fromASTBindingDecl (get rhs 1)) : fromASTTopLevelDeclSeq (get rhs 3) ),

      ("TopLevel -> DataTypeDecl",
        \rhs -> toASTTopLevelDeclSeq [DataTypeTopLevel (fromASTDataTypeDecl (get rhs 1))] ),

      ("TopLevel -> DataTypeDecl ; TopLevel",
        \rhs -> toASTTopLevelDeclSeq
            $ DataTypeTopLevel (fromASTDataTypeDecl (get rhs 1)) : (fromASTTopLevelDeclSeq (get rhs 3)) ),


      {- DataTypeDecl -}
      ("DataTypeDecl -> data identifier = DataTypeDeclRHS", \rhs ->
           let name = getText rhs 2
               (locvars,tyvars,tycondecls) = fromASTTriple (get rhs 4)
           in toASTDataTypeDecl (DataType name locvars tyvars tycondecls)),


      {- DataTypeDeclRHS -}
      ("DataTypeDeclRHS -> TypeConDecls", \rhs ->
           toASTTriple ([], [], fromASTTypeConDeclSeq (get rhs 1)) ),

      ("DataTypeDeclRHS -> { Identifiers } . DataTypeDeclRHS", \rhs ->
           let locvars = fromASTIdSeq (get rhs 2) in
           case fromASTTriple (get rhs 5) of
             ([], tyvars, tycondecls) -> toASTTriple (locvars, tyvars, tycondecls)
             (locvars', tyvars, tycondecls) ->
               error $ "[Parser] Not supported yet: multiple location abstractions: "
                           ++ show locvars' ++ " " ++ show locvars ),

      ("DataTypeDeclRHS -> [ Identifiers ] . DataTypeDeclRHS", \rhs ->
           let tyvars = fromASTIdSeq (get rhs 2) in
           case fromASTTriple (get rhs 5) of
             ([], [], tycondecls) -> toASTTriple ([], tyvars, tycondecls)
             (locvars, [], tycondecls) -> 
               error $ "Not supported yet: types and then locations abstractions: "
                           ++ show tyvars ++ " " ++ show locvars 
             (locvars, tyvars', tycondecls) ->
               error $ "Not supported yet: multiple type abstractions: "
                           ++ show tyvars' ++ " " ++ show tyvars ),


      {- TypeConDecl -}
      ("TypeConDecl -> identifier OptLocFunTypes",
        \rhs -> toASTTypeConDecl (TypeCon (getText rhs 1) (fromASTTypeSeq (get rhs 2))) ),


      {- TypeConDecls -}
      ("TypeConDecls -> TypeConDecl",
        \rhs -> toASTTypeConDeclSeq [ fromASTTypeConDecl (get rhs 1) ] ),

      ("TypeConDecls -> TypeConDecl | TypeConDecls",
        \rhs -> toASTTypeConDeclSeq $
                  fromASTTypeConDecl (get rhs 1) : fromASTTypeConDeclSeq (get rhs 3) ),


      {- Binding -}
      ("Binding -> identifier : Type = LExpr",
        \rhs -> toASTBindingDecl (
                  Binding (getText rhs 1) (fromASTType (get rhs 3)) (fromASTExpr (get rhs 5))) ),


      {- Bindings -}
      ("Bindings -> Binding",
        \rhs -> toASTBindingDeclSeq [ fromASTBindingDecl (get rhs 1) ] ),

      ("Bindings -> Binding ; Bindings",
        \rhs -> toASTBindingDeclSeq $ fromASTBindingDecl (get rhs 1) : fromASTBindingDeclSeq (get rhs 3) ),


      {- LExpr -}
      ("LExpr -> { Identifiers } . LExpr",
        \rhs -> toASTExpr (singleLocAbs (LocAbs (fromASTIdSeq (get rhs 2)) (fromASTExpr (get rhs 5)))) ),

      ("LExpr -> [ Identifiers ] . LExpr",
        \rhs -> toASTExpr (singleTypeAbs (TypeAbs (fromASTIdSeq (get rhs 2)) (fromASTExpr (get rhs 5)))) ),

      ("LExpr -> \\ IdTypeLocSeq . LExpr",
        \rhs -> toASTExpr (singleAbs (Abs (fromASTIdTypeLocSeq (get rhs 2)) (fromASTExpr (get rhs 4)))) ),

      ("LExpr -> let { Bindings } LExpr end",
        \rhs -> toASTExpr (Let (fromASTBindingDeclSeq (get rhs 3)) (fromASTExpr (get rhs 5))) ),

      ("LExpr -> if Expr then LExpr else LExpr",
        \rhs -> toASTExpr (Case (fromASTExpr (get rhs 2)) Nothing
                  [ Alternative trueLit  [] (fromASTExpr (get rhs 4))
                  , Alternative falseLit [] (fromASTExpr (get rhs 6)) ]) ),

      ("LExpr -> case Expr { Alternatives }",
        \rhs -> toASTExpr (Case (fromASTExpr (get rhs 2)) Nothing (fromASTAlternativeSeq (get rhs 4))) ),

      ("LExpr -> Expr", \rhs -> get rhs 1 ),


      {- IdTypeLocSeq -}
      ("IdTypeLocSeq -> IdTypeLoc", \rhs -> toASTIdTypeLocSeq [fromASTIdTypeLoc (get rhs 1)] ),

      ("IdTypeLocSeq -> IdTypeLoc IdTypeLocSeq",
        \rhs -> toASTIdTypeLocSeq $ fromASTIdTypeLoc (get rhs 1) : fromASTIdTypeLocSeq (get rhs 2) ),


      {- IdTypeLoc -}
      ("IdTypeLoc -> identifier : Type @ Location",
        \rhs -> toASTIdTypeLoc (getText rhs 1, fromASTType (get rhs 3), fromASTLocation (get rhs 5)) ),


      {- Alternatives -}
      ("Alternatives -> Alternative", \rhs -> toASTAlternativeSeq [fromASTAlternative (get rhs 1)] ),

      ("Alternatives -> Alternative ; Alternatives",
        \rhs -> toASTAlternativeSeq $ fromASTAlternative (get rhs 1) : fromASTAlternativeSeq (get rhs 3) ),


      {- Alternative -}
      ("Alternative -> identifier OptIdentifiers => LExpr",
        \rhs -> toASTAlternative $
                  (Alternative (getText rhs 1) (fromASTIdSeq (get rhs 2)) (fromASTExpr (get rhs 4))) ),

      ("Alternative -> ( OptIdentifierCommas ) => LExpr",
        \rhs -> toASTAlternative $
                  (TupleAlternative (fromASTIdSeq (get rhs 2)) (fromASTExpr (get rhs 5))) ),


      {- Expr -}
      ("Expr -> Expr Term",
        \rhs -> toASTExpr (App (fromASTExpr (get rhs 1)) Nothing (fromASTExpr (get rhs 2)) Nothing) ),

      ("Expr -> Expr [ LocFunTypes ]",
        \rhs -> toASTExpr (singleTypeApp (TypeApp (fromASTExpr (get rhs 1)) Nothing (fromASTTypeSeq (get rhs 3)))) ),

      ("Expr -> Expr { Identifiers }",
        \rhs -> toASTExpr (singleLocApp (LocApp (fromASTExpr (get rhs 1)) Nothing (map Location (fromASTIdSeq (get rhs 3))))) ),

      ("Expr -> Tuple", \rhs -> get rhs 1 ),

      ("Expr -> AssignExpr", \rhs -> get rhs 1 ),


      {- Tuple -}
      ("Tuple -> ( LExpr , LExprSeq )",
        \rhs -> toASTExpr (Tuple $ fromASTExpr (get rhs 2) : fromASTExprSeq (get rhs 4)) ),


      {- LExprSeq -}
      ("LExprSeq -> LExpr", \rhs -> toASTExprSeq [ fromASTExpr (get rhs 1) ] ),

      ("LExprSeq -> LExpr , LExprSeq",
        \rhs -> toASTExprSeq ( fromASTExpr (get rhs 1) : fromASTExprSeq (get rhs 3)) ),


      {- AssignExpr -}
      ("AssignExpr -> DerefExpr", \rhs -> get rhs 1 ),

      ("AssignExpr -> DerefExpr := { Identifiers } [ LocFunTypes ] AssignExpr",
       \rhs ->
         toASTExpr
         (App
          (App
           (singleTypeApp (TypeApp
            (singleLocApp ( LocApp (Var ":=")
                                   Nothing
                                   (map Location (fromASTIdSeq (get rhs 4))) ) )
            Nothing
            (fromASTTypeSeq (get rhs 7)) ) )
           Nothing
           (fromASTExpr (get rhs 1))
           Nothing )
          Nothing
          (fromASTExpr (get rhs 9))
          Nothing) ),


      {- DerefExpr -}
      ("DerefExpr -> LogicNot", \rhs -> get rhs 1 ),

      ("DerefExpr -> ! { Identifiers } [ LocFunTypes ] DerefExpr",
       \rhs ->
         toASTExpr
         (App
          (singleTypeApp (TypeApp
           (singleLocApp (LocApp (Var "!")
                                 Nothing
                                 (map Location (fromASTIdSeq (get rhs 3)))))
           Nothing
           (fromASTTypeSeq (get rhs 6)) ))
          Nothing
          (fromASTExpr (get rhs 8)) Nothing) ),

      ("DerefExpr -> LogicOr", \rhs -> get rhs 1 ),


      {- Expression operations -}
      ("LogicOr -> LogicOr or LogicAnd",
        \rhs -> toASTExpr (Prim OrPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("LogicOr -> LogicAnd", \rhs -> get rhs 1),

      ("LogicAnd -> LogicAnd and CompEqNeq",
        \rhs -> toASTExpr (Prim AndPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("LogicAnd -> CompEqNeq", \rhs -> get rhs 1),

      ("CompEqNeq -> CompEqNeq == Comp",
        \rhs -> toASTExpr (Prim EqPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("CompEqNeq -> CompEqNeq != Comp",
        \rhs -> toASTExpr (Prim NeqPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("CompEqNeq -> Comp", \rhs -> get rhs 1 ),

      ("Comp -> Comp < ArithAddSub",
        \rhs -> toASTExpr (Prim LtPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("Comp -> Comp <= ArithAddSub",
        \rhs -> toASTExpr (Prim LePrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("Comp -> Comp > ArithAddSub",
        \rhs -> toASTExpr (Prim GtPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("Comp -> Comp >= ArithAddSub",
        \rhs -> toASTExpr (Prim GePrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("Comp -> ArithAddSub", \rhs -> get rhs 1 ),

      ("ArithAddSub -> ArithAddSub + ArithMulDiv",
        \rhs -> toASTExpr (Prim AddPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("ArithAddSub -> ArithAddSub - ArithMulDiv",
        \rhs -> toASTExpr (Prim SubPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("ArithAddSub -> ArithMulDiv", \rhs -> get rhs 1 ),

      ("ArithMulDiv -> ArithMulDiv * ArithUnary",
        \rhs -> toASTExpr (Prim MulPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("ArithMulDiv -> ArithMulDiv / ArithUnary",
        \rhs -> toASTExpr (Prim DivPrimOp [] [] [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("ArithMulDiv -> ArithUnary", \rhs -> get rhs 1 ),

      ("ArithUnary -> - Term", \rhs -> toASTExpr (Prim NegPrimOp [] [] [fromASTExpr (get rhs 2)]) ),

      ("ArithUnary -> Term", \rhs -> get rhs 1 ),


      {- Term -}
      ("Term -> identifier", \rhs -> toASTExpr (Var (getText rhs 1)) ),

      ("Term -> integer", \rhs -> toASTExpr (Lit (IntLit (read (getText rhs 1)))) ),

      ("Term -> string", \rhs ->
          let str = read (getText rhs 1) :: String
          in  toASTExpr (Lit (StrLit str)) ),
      
      ("Term -> boolean", \rhs -> toASTExpr (Lit (BoolLit (read (getText rhs 1)))) ),

      ("Term -> ( )", \rhs -> toASTExpr (Lit UnitLit) ),

      ("Term -> ( LExpr )", \rhs -> get rhs 2 )
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe"
  }
