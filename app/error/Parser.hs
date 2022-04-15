module Parser where

import Attrs
import CommonParserUtil
import Token
import Expr

import Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.State.Lazy as ST

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule prec action = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token AST IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Expr'",

    tokenPrecAssoc =
    [ (Attrs.Nonassoc, [ "integer_number" ])   -- %token integer_number
    , (Attrs.Left,     [ "+", "-" ])           -- %left "+" "-"
    , (Attrs.Left,     [ "*", "/" ])           -- %left "*" "/"
    , (Attrs.Right,    [ "UMINUS" ])           -- %right UMINUS
    ],

    parserSpecList =
    [
      rule "Expr' -> Expr" (\rhs -> return $ get rhs 1),
      
      rule "Expr -> Expr + Expr"
        (\rhs -> return $ toAstExpr (
          BinOp Expr.ADD (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "Expr -> Expr - Expr"
        (\rhs -> return $ toAstExpr (
          BinOp Expr.SUB (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "Expr -> Expr * Expr"
        (\rhs -> return $ toAstExpr (
          BinOp Expr.MUL (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "Expr -> Expr / Expr"
        (\rhs -> return $ toAstExpr (
          BinOp Expr.DIV (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

      rule "Expr -> ( Expr )" (\rhs -> return $ get rhs 2),
      
      ruleWithPrec "Expr -> - Expr" "UMINUS"    -- Expr -> -Expr %prec UMINUS
        (\rhs -> return $ toAstExpr (
                             BinOp Expr.SUB (Lit 0) (fromAstExpr (get rhs 2))) ),
      
      rule "Expr -> integer_number"
        (\rhs -> return $ toAstExpr (Lit (read (getText rhs 1))) ),

      rule "Expr -> error"
        (\rhs -> do (_,line,col,text) <- ST.get
                    lift $ putStrLn $ "Expr -> error" ++ " at Line "
                                        ++ show line ++ ", Column " ++ show col
                    lift $ putStrLn $ " : " ++ take 77 text  -- 80 columns
                    return $ toAstExpr (Lit 0) )
      
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table.txt",  
    gotoTblFile    = "goto_table.txt",
    grammarFile    = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe   = "yapb-exe",

    synCompSpec = Nothing
  }


