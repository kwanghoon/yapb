
### How to write and run a parser using YAPB
~~~
  $ ls app/parser/*.hs
  app/parser/Lexer.hs  app/parser/Main.hs  app/parser/Parser.hs  app/parser/Token.hs

  $ cat app/parser/Lexer.hs
  module Lexer(lexerSpec) where

  import Prelude hiding (EQ)
  import CommonParserUtil
  import Token

  mkFn :: Token -> (String -> Maybe Token)
  mkFn tok = \text -> Just tok

  skip :: String -> Maybe Token
  skip = \text -> Nothing

  lexerSpec :: LexerSpec Token
  lexerSpec = LexerSpec
    {
      endOfToken    = END_OF_TOKEN,
      lexerSpecList = 
        [ ("[ \t\n]", skip),
          ("[0-9]+" , mkFn INTEGER_NUMBER),
          ("\\("    , mkFn OPEN_PAREN),
          ("\\)"    , mkFn CLOSE_PAREN),
          ("\\+"    , mkFn ADD),
          ("\\-"    , mkFn SUB),
          ("\\*"    , mkFn MUL),
          ("\\/"    , mkFn DIV),
          ("\\="    , mkFn EQ),
          ("\\;"    , mkFn SEMICOLON),
          ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
        ]
    } 


  $ cat app/parser/Parser.hs
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

  $ cat app/parser/example/oneline.arith
  1 + 2 - 3 * 4 / 5
  
  $ cat app/parser/example/multiline.arith
  x = 123;
  x = x + 1;
  y = x; 
  y = y - 1 * 2 / 3;
  z = y = x

  $ stack exec parser-exe
  Enter your file: app/parser/example/oneline.arith
  Lexing...
  Parsing...
  done.
  Pretty Printing...
  ((1 + 2) - ((3 * 4) / 5))
  
  $ stack exec parser-exe
  Enter your file: app/parser/example/multiline.arith
  Lexing...
  Parsing...
  done.
  Pretty Printing...
  (x = 123); (x = (x + 1)); (y = x); (y = (y - ((1 * 2) / 3))); (z = (y = x))
~~~

