module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token AST
parserSpec = ParserSpec
  {
    startSymbol = "SeqExpr'",

    tokenPrecAssoc = [],
    
    parserSpecList =
    [
      rule "SeqExpr' -> SeqExpr" (\rhs -> get rhs 1),

      rule "SeqExpr -> SeqExpr ; AssignExpr"
        (\rhs -> toAstSeq (
          fromAstSeq (get rhs 1) ++ [fromAstExpr (get rhs 3)]) ),
      
      rule "SeqExpr -> AssignExpr" (\rhs -> toAstSeq [fromAstExpr (get rhs 1)]),
      
      rule "AssignExpr -> identifier = AssignExpr"
        (\rhs -> toAstExpr (Assign (getText rhs 1) (fromAstExpr (get rhs 3))) ),
      
      rule "AssignExpr -> AdditiveExpr" (\rhs -> get rhs 1),

      rule "AdditiveExpr -> AdditiveExpr + MultiplicativeExpr"
        (\rhs -> toAstExpr (
          BinOp Expr.ADD (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "AdditiveExpr -> AdditiveExpr - MultiplicativeExpr"
        (\rhs -> toAstExpr (
          BinOp Expr.SUB (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "AdditiveExpr -> MultiplicativeExpr" (\rhs -> get rhs 1),

      rule "MultiplicativeExpr -> MultiplicativeExpr * PrimaryExpr"
        (\rhs -> toAstExpr (
          BinOp Expr.MUL (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "MultiplicativeExpr -> MultiplicativeExpr / PrimaryExpr"
        (\rhs -> toAstExpr (
          BinOp Expr.DIV (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "MultiplicativeExpr -> PrimaryExpr" (\rhs -> get rhs 1),
      
      rule "PrimaryExpr -> identifier" (\rhs -> toAstExpr (Var (getText rhs 1)) ),

      rule "PrimaryExpr -> integer_number"
        (\rhs -> toAstExpr (Lit (read (getText rhs 1))) ),

      rule "PrimaryExpr -> ( AssignExpr )" (\rhs -> get rhs 2)
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table.txt",  
    gotoTblFile    = "goto_table.txt",
    grammarFile    = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe   = "yapb-exe"
  }


