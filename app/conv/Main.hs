module Main where

import CFG
import ReadGrammar

import System.Environment (getArgs)

-- How to run:
--    $ stack exec conv-exe grm/polyrpc.lgrm

main = test conversion

test fun = do
  args <- getArgs
  repTest fun args

repTest fun [] = return ()
repTest fun (arg:args) = do
  text <- readFile arg
  fun text
  repTest fun args

