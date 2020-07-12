
## Writing your parser using YAPB

### How to configure a haskell Stack project using YAPB

In the beginning, you initialize a haskell Stack project.

~~~
  $ stack new parser
  $ cd parser
~~~

To use the function of automatic computation of syntax completion candidates, the project must use YAPB 0.1.1. 

[*package.yaml* of *arith*]  
~~~
executables:
  arith-exe:
    main:                Main.hs
    source-dirs:
    - app/ast         <====== To include ast/Expr.hs
    - app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - arith
    - yapb >= 0.1.1    <====== Add a dependency on yapb-0.1.1!

~~~

Note that YAPB-0.1.0 is available at Hackage, but YAPB-0.1.1 is not. So, you have to git-clone the yapb repository together, and *stack.yaml* of *arith* needs a change as follows.
[*stack.yaml* in *arith*]
~~~
packages:
- .
- ../yapb    <====== To refer to yapb-0.1.1 assuming the directory of yapb is in the same level. Otherwise, you may adjust the path.
~~~
 - This inconvenience will disappear after yapb-0.1.1 is registered at Hackage. 


### How to write and run a parser using YAPB

For your own parser, you have to write *Token.hs*, *Lexer.hs*, *ast/Expr.hs*, *Parser.hs*, and *Main.hs*.

~~~
  $ ls app/*.hs app/ast/*.hs
  app/Lexer.hs  app/Main.hs  app/Parser.hs  app/Token.hs app/ast/Expr.hs
~~~

How to write those files are explained in the followin. 


[app/Token.hs]
~~~
  module Token(Token(..)) where

  import Prelude hiding(EQ)
  import TokenInterface

  -----------------------------------------------
  -- What you need to write for your own parser!!
  -----------------------------------------------
  data Token =
      END_OF_TOKEN
    | OPEN_PAREN  | CLOSE_PAREN
    | IDENTIFIER  | INTEGER_NUMBER
    | ADD  | SUB  | MUL  | DIV
    | EQ  | SEMICOLON
    deriving (Eq, Show)

  tokenStrList :: [(Token,String)]
  tokenStrList =
    [ (END_OF_TOKEN, "$"),
      (OPEN_PAREN, "("), (CLOSE_PAREN, ")"),
      (IDENTIFIER, "identifier"), (INTEGER_NUMBER, "integer_number"),
      (ADD, "+"), (SUB, "-"), (MUL, "*"), (DIV, "/"),
      (EQ, "="), (SEMICOLON, ";")  
    ]

  -------------------------------------
  -- Just copy the following as it is!!
  -------------------------------------
  findTok tok [] = Nothing
  findTok tok ((tok_,str):list)
    | tok == tok_ = Just str
    | otherwise   = findTok tok list

  findStr str [] = Nothing
  findStr str ((tok,str_):list)
    | str == str_ = Just tok
    | otherwise   = findStr str list
  
  instance TokenInterface Token where
    toToken str   =
      case findStr str tokenStrList of
        Nothing  -> error ("toToken: " ++ str)
        Just tok -> tok
    fromToken tok =
      case findTok tok tokenStrList of
        Nothing  -> error ("fromToken: " ++ show tok)
        Just str -> str

~~~

[app/Lexer.hs]
~~~
  module Lexer(lexerSpec) where

  import Prelude hiding (EQ)
  import CommonParserUtil
  import Token

  ----------------------------
  -- Utility functions
  ----------------------------
  
  mkFn :: Token -> (String -> Maybe Token)
  mkFn tok = \text -> Just tok

  skip :: String -> Maybe Token
  skip = \text -> Nothing

  ----------------------------
  -- Your lexer specification
  ----------------------------
  
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
~~~

[app/ast/Expr.hs]
~~~
  module Expr where

  -----------------------------------------------
  -- What you need to write for your own parser!!
  -----------------------------------------------

  data AST =
      ASTSeq  { fromAstSeq  :: [Expr] } -- Expr Sequence: Expr1; ... ; Exprn
    | ASTExpr { fromAstExpr :: Expr   }

  instance Show AST where
    showsPrec p _ = (++) "AST ..."

  toAstSeq :: [Expr] -> AST
  toAstSeq exprs = ASTSeq exprs

  toAstExpr :: Expr -> AST
  toAstExpr expr = ASTExpr expr

  data Expr =
      Lit { fromLit :: Int }
    | Var { fromVar :: String }
    | BinOp { kindFromBinOp :: BinOpKind,
              leftOpFromBinOp :: Expr,
              rightOpFromBinOp :: Expr }
    | Assign { lhsFromAssign :: String,
               rhsFromAssign :: Expr  }

  data BinOpKind = ADD | SUB | MUL | DIV

  ---------------------------------------
  -- Optional: for pretty printing an AST
  ---------------------------------------
  pprintAst :: AST -> String
  pprintAst (ASTSeq exprs) =
    let insSemicolon []         = ""
        insSemicolon [str]      = str
        insSemicolon (str:strs) = str ++ "; " ++ insSemicolon strs
    in insSemicolon (map pprint exprs)
    
  pprintAst (ASTExpr expr) = pprint expr

  pprint :: Expr -> String
  pprint (Lit i) = show i
  pprint (Var v) = v
  pprint (BinOp Expr.ADD left right) =
    "(" ++ pprint left ++ " + " ++ pprint right ++ ")"
  pprint (BinOp Expr.SUB left right) =
    "(" ++ pprint left ++ " - " ++ pprint right ++ ")"
  pprint (BinOp Expr.MUL left right) =
    "(" ++ pprint left ++ " * " ++ pprint right ++ ")"
  pprint (BinOp Expr.DIV left right) =
    "(" ++ pprint left ++ " / " ++ pprint right ++ ")"
  pprint (Assign x expr) =   
    "(" ++ x ++ " = " ++ pprint expr ++ ")"
~~~

[app/Parser.hs]
~~~
  module Parser where

  import CommonParserUtil
  import Token
  import Expr


  ----------------------------
  -- Your parser specification
  ----------------------------
  
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
~~~

[app/Main.hs]
~~~
  module Main where

  import CommonParserUtil

  import Lexer
  import Terminal
  import Parser
  import Expr

  import System.IO

  main :: IO ()
  main = do
    fileName <- readline "Enter your file: "
    case fileName of
      "exit" -> return ()
      line -> doProcess line

  doProcess line = do
    text <- readFile line 
    putStrLn "Lexing..."
    terminalList <- lexing lexerSpec text
    putStrLn "Parsing..."
    exprSeqAst <- parsing parserSpec terminalList
    putStrLn "Pretty Printing..."
    putStrLn (pprintAst exprSeqAst)
  
  
  readline msg = do
    putStr msg
    hFlush stdout
    readline'

  readline' = do
    ch <- getChar
    if ch == '\n' then
      return ""
    else
      do line <- readline'
         return (ch:line)
~~~

Now you are ready to run the arith parser. For testing, assume you write two examples, *oneline.arith* and *multiline.arith*.

[app/example/oneline.arith]
~~~
  1 + 2 - 3 * 4 / 5
~~~

[app/example/multiline.arith]
~~~
  x = 123;
  x = x + 1;
  y = x; 
  y = y - 1 * 2 / 3;
  z = y = x
~~~

Run the arith parser by stack. 
~~~
  $ stack exec parser-exe
  Enter your file: app/example/oneline.arith
  Lexing...
  Parsing...
  done.
  Pretty Printing...
  ((1 + 2) - ((3 * 4) / 5))
  
  $ stack exec parser-exe
  Enter your file: app/example/multiline.arith
  Lexing...
  Parsing...
  done.
  Pretty Printing...
  (x = 123); (x = (x + 1)); (y = x); (y = (y - ((1 * 2) / 3))); (z = (y = x))
~~~

