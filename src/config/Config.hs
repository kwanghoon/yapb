module Config where

import Data.Maybe
import System.IO
import System.Directory

data Configuration =
  Configuration {
    config_SIMPLE :: Bool,    -- Simple(True), Nested(False)
    config_R_LEVEL :: Int,    -- 
    config_GS_LEVEL :: Int,   -- 
    config_DEBUG :: Bool,     -- True => Debugging on, False => Debugging off
    config_DISPLAY :: Bool    -- True => ...,          False => Nonterminal name
  }
  deriving (Read, Show)

configFileName = "yapb.config"

readConfig :: IO (Maybe Configuration)
readConfig =
  do exists <- doesFileExist configFileName
     if exists
       then do text <- readFile configFileName
               return (Just $ (read text :: Configuration))
       else return Nothing

     
