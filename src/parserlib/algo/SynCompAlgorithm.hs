module SynCompAlgorithm(chooseCompCandidatesFn, defaultCompCandidatesFn) where

import qualified SynCompAlgoBU as BU
import qualified SynCompAlgoTD as TD
import qualified SynCompAlgoPEPM as PEPM

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
       Just config -> return (choose (config_ALGORITHM config))
  where
    choose 0 = BU.compCandidates
    choose 1 = TD.compCandidates
    choose 2 = PEPM.compCandidates
    choose _ = defaultCompCandidatesFn  -- by default !

defaultCompCandidatesFn :: (TokenInterface token, Typeable token, Typeable ast, Show token, Show ast) =>
   CompCandidates token ast
        -> Int
        -> [Candidate]
        -> Int
        -> Stack token ast
        -> IO ([[Candidate]], Bool)
defaultCompCandidatesFn = BU.compCandidates

