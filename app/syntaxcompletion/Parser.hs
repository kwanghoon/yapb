module Parser where

import CommonParserUtil
import Token

data AST = AST  -- We do not build any ASTs!!
     deriving (Show)

parserSpec :: ParserSpec Token AST
parserSpec = ParserSpec
  {
    startSymbol = "Start'",
    
    parserSpecList =
    [
      ("Start' -> Start", \rhs -> get rhs 1),

      ("Start -> Exp", \rhs -> get rhs 1),

      ("Exp -> AppExp", \rhs -> get rhs 1),

      ("Exp -> fn identifier => Exp", \rhs -> AST),

      ("AppExp -> AtExp", \rhs -> get rhs 1),

      ("AppExp -> AppExp AtExp", \rhs -> AST),

      ("AtExp -> identifier", \rhs -> AST),

      ("AtExp -> ( Exp )", \rhs -> AST),

      ("AtExp -> let Dec in Exp end", \rhs -> AST),

      ("Dec -> val identifier = Exp", \rhs -> AST)
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "genlrparser-exe"
  }
