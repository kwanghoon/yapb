module Config where

import Text.Read (readMaybe)
import Data.Maybe
import System.IO
import System.Directory

data Configuration =
  Configuration {
    config_SIMPLE :: Bool,      -- Simple(True), Nested(False)
    config_R_LEVEL :: Int,      -- 
    config_GS_LEVEL :: Int,     -- 
    config_DEBUG :: Bool,       -- True => Debugging on, False => Debugging off
    config_DISPLAY :: Bool,     -- True => Display in Emacs, False => Do not display in Emacs
    config_PRESENTATION :: Int  -- 0 : default, 1 : ...
  }
  deriving (Read, Show)

configFileName = "yapb.config"

readConfig :: IO (Maybe Configuration)
readConfig =
  do exists <- doesFileExist configFileName
     if exists
       then do text <- readFile configFileName
               case  readMaybe text :: Maybe Configuration of
                 Just config_text -> return $ Just config_text
                 Nothing -> error $ "readConfig: unexpected configuration\n" ++ show text
       else return Nothing

     
