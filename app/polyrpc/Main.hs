{-# LANGUAGE DeriveGeneric #-}

module Main where

import CommonParserUtil

import Token
import Lexer
import Terminal
import Parser
import Type
import Expr
import qualified CSType as TT
import qualified CSExpr as TE
import TypeCheck
import Compile
import Verify
import Execute

import Text.JSON.Generic
import Text.JSON.Pretty
import Text.PrettyPrint
-- For aeson
--import qualified Data.ByteString.Lazy.Char8 as B
--import Data.Aeson.Encode.Pretty
import Data.Maybe
import System.IO 
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  cmd  <- getCmd args
  
  let files = _files cmd
  
  mapM_ (doProcess cmd) files -- [ ((build cmd file), file) | file <- files ]

doProcess cmd file = do
  putStrLn $ "[Reading] " ++ file
  text <- readFile file

  putStrLn "[Lexing]"
  terminalList <- lexing lexerSpec text
  verbose (_flag_debug_lex cmd) $ mapM_ (putStrLn) (map terminalToString terminalList)


  putStrLn "[Parsing]"
  exprSeqAst <- parsing parserSpec terminalList
  
  verbose (_flag_debug_parse cmd) $ putStrLn "Dumping..."
  verbose (_flag_debug_parse cmd) $ putStrLn $ show $ fromASTTopLevelDeclSeq exprSeqAst
  
  let toplevelDecls = fromASTTopLevelDeclSeq exprSeqAst

  
  putStrLn "[Type checking]"
  (gti, elab_toplevelDecls) <- typeCheck toplevelDecls
  verbose (_flag_debug_typecheck cmd) $ putStrLn "Dumping..."
  verbose (_flag_debug_typecheck cmd) $ putStrLn $ show $ elab_toplevelDecls

  print_rpc cmd file elab_toplevelDecls


  putStrLn "[Compiling]"
  (t_gti, funStore, t_expr) <- compile gti elab_toplevelDecls
  verbose (_flag_debug_compile cmd) $ putStrLn "Dumping...\nGlobal type information:\n"
  verbose (_flag_debug_compile cmd) $ putStrLn $ (show t_gti ++ "\n\nFunction stores:")
  verbose (_flag_debug_compile cmd) $ putStrLn $ (show funStore ++ "\n\nMain expression:")
  verbose (_flag_debug_compile cmd) $ putStrLn $ (show t_expr ++ "\n")

  print_cs cmd file funStore t_expr

  putStrLn "[Verifying generated codes]"
  verify t_gti funStore t_expr
  verbose (_flag_debug_verify cmd) $ putStrLn "[Well-typed]"

  putStrLn "[Executing codes]"
  v <- execute (_flag_debug_run cmd) t_gti funStore t_expr
  verbose (_flag_debug_run cmd) $ putStrLn $ "[Result]\n" ++ show v

  putStrLn "[Success]"


--
print_rpc cmd file elab_toplevelDecls = do
  let jsonfile = prefixOf file ++ ".json"
  if _flag_print_rpc_json cmd
  then do putStrLn $ "Writing to " ++ jsonfile
          writeFile jsonfile $ render
             $ pp_value $ toJSON (elab_toplevelDecls :: [TopLevelDecl])
  else return ()

print_cs cmd file funStore t_expr = do
  let jsonfile = prefixOf file ++ "_cs.json"
  if _flag_print_cs_json cmd
  then do putStrLn $ "Writing to " ++ jsonfile
          writeFile jsonfile $ render
             $ pp_value $ toJSON (funStore :: TE.FunctionStore, t_expr :: TE.Expr)
  else return ()

prefixOf str = reverse (removeDot (dropWhile (/='.') (reverse str)))
  where removeDot []     = []
        removeDot (x:xs) = xs  -- x must be '.'

--
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

--
data Cmd =
  Cmd { _flag_print_rpc_json :: Bool
      , _flag_print_cs_json :: Bool
      , _flag_debug_lex :: Bool
      , _flag_debug_parse :: Bool
      , _flag_debug_typecheck :: Bool
      , _flag_debug_compile :: Bool
      , _flag_debug_verify :: Bool
      , _flag_debug_run :: Bool
      , _files :: [String]
      }

initCmd =
  Cmd { _flag_print_rpc_json = False
      , _flag_print_cs_json  = False
      , _flag_debug_lex = False
      , _flag_debug_parse = False
      , _flag_debug_typecheck = False
      , _flag_debug_compile = False
      , _flag_debug_verify = False
      , _flag_debug_run = False
      , _files = []
      }

getCmd :: Monad m => [String] -> m Cmd
getCmd args = collect initCmd args 

collect :: Monad m => Cmd -> [String] -> m Cmd
collect cmd [] = return cmd
collect cmd ("--output-json":args) = do
  let new_cmd = cmd { _flag_print_rpc_json = True }
  collect new_cmd args
  
collect cmd ("--output-rpc-json":args) = do  
  let new_cmd = cmd { _flag_print_rpc_json = True }
  collect new_cmd args
  
collect cmd ("--output-cs-json":args) = do  
  let new_cmd = cmd { _flag_print_cs_json = True }
  collect new_cmd args

collect cmd ("--debug-lex":args) = do    
  let new_cmd = cmd { _flag_debug_lex = True }
  collect new_cmd args
  
collect cmd ("--debug-parse":args) = do    
  let new_cmd = cmd { _flag_debug_parse = True }
  collect new_cmd args
  
collect cmd ("--debug-typecheck":args) = do    
  let new_cmd = cmd { _flag_debug_typecheck = True }
  collect new_cmd args
  
collect cmd ("--debug-compile":args) = do    
  let new_cmd = cmd { _flag_debug_compile = True }
  collect new_cmd args
  
collect cmd ("--debug-verify":args) = do    
  let new_cmd = cmd { _flag_debug_verify = True }
  collect new_cmd args
  
collect cmd ("--debug-run":args) = do    
  let new_cmd = cmd { _flag_debug_run = True }
  collect new_cmd args
  
collect cmd (arg:args) = do
  let old_files = _files cmd 
  let new_cmd = cmd { _files = old_files ++ [arg] }
  collect new_cmd args

  
verbose b action = if b then action else return ()
