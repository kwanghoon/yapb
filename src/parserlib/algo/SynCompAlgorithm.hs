module SynCompAlgorithm(chooseCompCandidatesFn, defaultCompCandidatesFn) where

import qualified SynCompAlgoBU as BU
import qualified SynCompAlgoTD as TD
import qualified SynCompAlgoPEPM as PEPM

-- For experiment
import qualified SynCompAlgoBUTree as BUTree
import qualified SynCompAlgoBUTreeNested as BUTreeNested

import TokenInterface
import Config
import SynCompAlgoUtil
import CommonParserUtil

import Data.Typeable

chooseCompCandidatesFn :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
  IO (  CompCandidates token ast
        -> Int
        -> [Candidate]
        -> Int
        -> Stack token ast
        -> IO ([[Candidate]], Bool) )
chooseCompCandidatesFn =
  do maybeConfig <- readConfig
     case maybeConfig of
       Nothing -> return defaultCompCandidatesFn
       Just config -> return (choose (config_ALGORITHM config) (config_SIMPLE config))
  where
    choose 0 _     = BU.compCandidates
    choose 1 _     = TD.compCandidates
    choose 2 _     = PEPM.compCandidates
    choose 3 True  = BUTree.compCandidates         -- 3 and Simple
    choose 3 False = BUTreeNested.compCandidates   -- 3 and Nested
    choose _ _     = defaultCompCandidatesFn  -- by default !

defaultCompCandidatesFn :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
   CompCandidates token ast
        -> Int
        -> [Candidate]
        -> Int
        -> Stack token ast
        -> IO ([[Candidate]], Bool)
defaultCompCandidatesFn = BU.compCandidates

