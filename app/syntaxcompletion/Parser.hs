module Parser where

import CommonParserUtil
import Token
import Expr
import Lexer

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

noAction = \rhs -> return ()

--
parserSpec :: ParserSpec Token AST IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Start'",
    
    tokenPrecAssoc = [],

    chumLexerSpec = lexerSpec,
    
    parserSpecList =
    [
      rule "Start' -> Start" noAction,

      rule "Start -> Exp" noAction,

      rule "Exp -> AppExp" noAction,

      rule "Exp -> fn identifier => Exp" noAction,

      rule "AppExp -> AtExp" noAction,

      rule "AppExp -> AppExp AtExp" noAction,

      rule "AtExp -> identifier" noAction,

      rule "AtExp -> ( Exp )" noAction,

      rule "AtExp -> let Dec in Exp end" noAction,

      rule "Dec -> val identifier = Exp" noAction
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe"
  }
