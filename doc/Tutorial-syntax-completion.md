## Syntax completion server for Emacs using YAPB
Three steps
- Step 1: Prepare your parser
- Step 2: Add a syntax completion server function to the parser
- Step 3: Run Emacs as your text editor to write a program text, to configure Emacs, and to type TAB for syntax completion

Two reference projects
 - [arith](https://github.com/kwanghoon/arith)
 - [smllike](https://github.com/kwanghoon/smllike)

In the following, we choose *arith* as an example of using YAPB for developing a syntax completion server for a very simple arithmetic expression language.

### Initialize a Haskell project using [Stack](https://docs.haskellstack.org/en/stable/README/)
~~~
$ stack new arith
$ cd arith
~~~

### Prepare your parser and a driver

A list of files for a parser together with a main module 
- for parser:
   * [app/Token.hs](https://github.com/kwanghoon/arith/blob/master/app/Token.hs)
   * [app/Lexer.hs](https://github.com/kwanghoon/arith/blob/master/app/Lexer.hs)
   * [app/ast/Expr.hs](https://github.com/kwanghoon/arith/blob/master/app/ast/Expr.hs)
   * [app/Parser.hs](https://github.com/kwanghoon/arith/blob/master/app/Parser.hs)
- for a driver for a syntax completion server
   * app/Main.hs 

~~~
$ ls app/*.hs app/ast/Expr.hs
app/Lexer.hs  app/Main.hs  app/Parser.hs  app/Token.hs  app/ast/Expr.hs
~~~

For a parser, you may refer to another tutorial on how to write a parser using YAPB. The exatcly same source code for the parser is used for computing syntax completion candidates. 
- Token.hs, ast/Expr.hs, Lexer.hs, and Parser.hs

For a driver, you may use the following code as it is. 
 - Main.hs
~~~
module Main where

import CommonParserUtil
import Token
import Expr
import Lexer
import Parser
import EmacsServer
import SynCompInterface
import Control.Exception
import System.IO

main :: IO ()
main = do
 emacsServer computeCand      -- *main* calls *emacsServer* with *computeCand* as an argument.

computeCand :: String -> Bool -> IO [EmacsDataItem]
computeCand programTextUptoCursor isSimpleMode = ((do
 --------------------------------------------------------------------------------------------
 -- The lexer and the parser are working
 --------------------------------------------------------------------------------------------
 
 terminalList <- lexing lexerSpec programTextUptoCursor   
 ast <- parsing parserSpec terminalList                   
 
 --------------------------------------------------------------------------------------------
 -- In case of no exception, the program text is successfully parsed
 --------------------------------------------------------------------------------------------
 
 successfullyParsed)                                      
 
 --------------------------------------------------------------------------------------------
 -- In case of *LexError*, there is an unacceptable lexical symbol. The exception is handled 
 -- by handleLexError.
 --------------------------------------------------------------------------------------------
 
 `catch` \e -> case e :: LexError of _ -> handleLexError)

 --------------------------------------------------------------------------------------------
 -- In case of *ParseError*, the driver calls *handleParseError* to start computing candidates 
 -- in the mode specified by *isSimpleMode*. The exception *e* contains all  LALR(1) automaton 
 -- state information (automaton stack, action table, goto table, production rules) at the time 
 -- that the parsing stopped. 
 --------------------------------------------------------------------------------------------
 
 `catch` \e -> case e :: ParseError Token AST of _ -> handleParseError isSimpleMode e
~~~

### Configure the Haskell project to use YAPB
To use the function of automatic computation of syntax completion candidates, the project must use YAPB >=0.1.1. 

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

Note that *stack.yaml* of *arith* needs a change to download yapb-0.1.1 from Hackage as follows.

[*stack.yaml* in *arith*]
~~~
extra-deps:
  - yapb-0.1.1
~~~


### Build arith and run a server
To use the function of automatic computation of syntax completion candidates, the project must use YAPB >=0.1.1. 

~~~
$ stack clean
$ stack build
$ stack exec arith-exe     <====== To start a server
~~~

You need to have three Emacs scripts: load.el, popup.el, and syntaxcompletion-mode.el. You can simply copy these three files from yapb/app/syntaxcompletion/examples/{load,popup,syntaxcompletion-mode}.el to your directory, say, app/example/.

Then run Emacs to open a program file, say, *app/example/test1.arith*. The following commands are available.
- *M-x load-file*, to enter *load.el* in the same directory (app/example/)
- *M-x syntaxcomplete-mode* for a simple mode
- *M-x syntaxcomplete-mode-nested* for a nested mode

Now you are assumed to edit the program text.  
~~~
$ cat app/example/test1.arith 
(2 + 3
~~~
- Move the cursor  to the end of the program text, *( 2 + 3*, and
- Press TAB to display candidates for syntax completion.

You must see a list of candidates in Emacs window as long as no problem happens. 

