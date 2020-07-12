module Parser where

import CommonParserUtil
import Token
import Expr

noAction = \rhs -> ()

parserSpec :: ParserSpec Token AST
parserSpec = ParserSpec
  {
    startSymbol = "Start'",
    
    parserSpecList =
    [
      ("Start' -> Start", noAction),

      ("Start -> Exp", noAction),

      ("Exp -> AppExp", noAction),

      ("Exp -> fn identifier => Exp", noAction),

      ("AppExp -> AtExp", noAction),

      ("AppExp -> AppExp AtExp", noAction),

      ("AtExp -> identifier", noAction),

      ("AtExp -> ( Exp )", noAction),

      ("AtExp -> let Dec in Exp end", noAction),

      ("Dec -> val identifier = Exp", noAction)
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe"
  }
