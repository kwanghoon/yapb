module Parser where

import CommonParserUtil
import Token
import Expr


parserSpec :: ParserSpec Token AST
parserSpec = ParserSpec
  {
    startSymbol = "SeqExpr'",
    
    parserSpecList =
    [
      ("SeqExpr' -> SeqExpr", \rhs -> get rhs 1),

      ("SeqExpr -> SeqExpr ; AssignExpr",
        \rhs -> toAstSeq (
          fromAstSeq (get rhs 1) ++ [fromAstExpr (get rhs 3)]) ),
      
      ("SeqExpr -> AssignExpr", \rhs -> toAstSeq [fromAstExpr (get rhs 1)]),
      
      ("AssignExpr -> identifier = AssignExpr",
        \rhs -> toAstExpr (Assign (getText rhs 1) (fromAstExpr (get rhs 3))) ),
      
      ("AssignExpr -> AdditiveExpr", \rhs -> get rhs 1),

      ("AdditiveExpr -> AdditiveExpr + MultiplicativeExpr",
        \rhs -> toAstExpr (
          BinOp Expr.ADD (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      ("AdditiveExpr -> AdditiveExpr - MultiplicativeExpr",
        \rhs -> toAstExpr (
          BinOp Expr.SUB (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      ("AdditiveExpr -> MultiplicativeExpr", \rhs -> get rhs 1),

      ("MultiplicativeExpr -> MultiplicativeExpr * PrimaryExpr",
        \rhs -> toAstExpr (
          BinOp Expr.MUL (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      ("MultiplicativeExpr -> MultiplicativeExpr / PrimaryExpr",
        \rhs -> toAstExpr (
          BinOp Expr.DIV (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      ("MultiplicativeExpr -> PrimaryExpr", \rhs -> get rhs 1),
      
      ("PrimaryExpr -> identifier", \rhs -> toAstExpr (Var (getText rhs 1)) ),

      ("PrimaryExpr -> integer_number",
        \rhs -> toAstExpr (Lit (read (getText rhs 1))) ),

      ("PrimaryExpr -> ( AssignExpr )", \rhs -> get rhs 2)
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe"
  }


