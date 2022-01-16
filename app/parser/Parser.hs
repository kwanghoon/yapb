module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token AST IO ()
parserSpec = ParserSpec
  {
    startSymbol = "SeqExpr'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      rule "SeqExpr' -> SeqExpr" (\rhs -> return $ get rhs 1),

      rule "SeqExpr -> SeqExpr ; AssignExpr"
        (\rhs -> return $ toAstSeq (
                            fromAstSeq (get rhs 1) ++ [fromAstExpr (get rhs 3)]) ),
      
      rule "SeqExpr -> AssignExpr" (\rhs -> return $ toAstSeq [fromAstExpr (get rhs 1)]),
      
      rule "AssignExpr -> identifier = AssignExpr"
        (\rhs -> return $ toAstExpr (Assign (getText rhs 1) (fromAstExpr (get rhs 3))) ),
      
      rule "AssignExpr -> AdditiveExpr" (\rhs -> return $ get rhs 1),

      rule "AdditiveExpr -> AdditiveExpr + MultiplicativeExpr"
        (\rhs -> return $ toAstExpr (
          BinOp Expr.ADD (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "AdditiveExpr -> AdditiveExpr - MultiplicativeExpr"
        (\rhs -> return $ toAstExpr (
          BinOp Expr.SUB (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "AdditiveExpr -> MultiplicativeExpr" (\rhs -> return $ get rhs 1),

      rule "MultiplicativeExpr -> MultiplicativeExpr * PrimaryExpr"
        (\rhs -> return $ toAstExpr (
          BinOp Expr.MUL (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "MultiplicativeExpr -> MultiplicativeExpr / PrimaryExpr"
        (\rhs -> return $ toAstExpr (
          BinOp Expr.DIV (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "MultiplicativeExpr -> PrimaryExpr" (\rhs -> return $ get rhs 1),
      
      rule "PrimaryExpr -> identifier" (\rhs -> return $ toAstExpr (Var (getText rhs 1)) ),

      rule "PrimaryExpr -> integer_number"
        (\rhs -> return $ toAstExpr (Lit (read (getText rhs 1))) ),

      rule "PrimaryExpr -> ( AssignExpr )" (\rhs -> return $ get rhs 2)
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table.txt",  
    gotoTblFile    = "goto_table.txt",
    grammarFile    = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe   = "yapb-exe"
  }


