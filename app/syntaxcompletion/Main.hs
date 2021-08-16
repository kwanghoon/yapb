module Main where

import EmacsServer

import SyntaxCompletion (computeCand)
import SyntaxCompletionSpec (spec)

import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  args <- getArgs
  if "test" `elem` args
    then withArgs [] spec
    else emacsServer computeCand
