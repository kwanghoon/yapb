
## YAPB: Yet Another Parser Builder

### A programmable parser builder system
- Allows to write LALR(1) parser specifications in Haskell
- Provides an automatic syntax completion method

### Library, tools, and examples
- yapb: a library for a programmable parser builder system
- yapb-exe: a wrapper interface to YAPB
- conv-exe: a grammar format utility for conversion of a readable grammar (.lgrm) format into the Haskell data format (.grm)
- syncomp-exe: a syntax completion server for Emacs
- Examples: 
  - parser-exe: an arithmetic parser
  - polyrpc-exe: a polyrpc programming language system including a parser, a poly rpc type checker, a slicing compiler, a poly cs type checker, and a poly cs interpter.

### Download and build
~~~
  $ git clone https://github.com/kwanghoon/yapb
  $ cd yapb
  $ stack build
~~~

### How to write and run a parser
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

### Documents
- [Parser generators sharing LR automaton generators and accepting general-purpose programming language-based specifications, J. of KIISE, 47(1), January 2020](http://swlab.jnu.ac.kr/paper/kiise202001.pdf) Written in Korean.
- [A topdown approach to writing a compiler](https://github.com/kwanghoon/swlab_parser_builder/blob/master/doc/tutorial_swlab_parser_builder.txt) Written in Korean.
- C++/Java/Python parser builder systems using YAPB
  - [Java parser](https://github.com/kwanghoon/swlab_parser_builder)
  - [C++ parser](https://github.com/tlsdorye/swlab-parser-lib)
  - [Python parser](https://github.com/limjintack/swlab_parser_python).
  - Architecture
    * <img src="https://github.com/kwanghoon/genlrparser/blob/master/doc/parsertoolarchitecture.png"/>

