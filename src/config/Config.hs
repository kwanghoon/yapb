module Config where

import Text.Read (readMaybe)
import System.Directory ( doesFileExist )
import qualified Data.Functor as True

-- | Configurations

-- config_SIMPLE ::= True | False
--       True for the simple algorithms, False for the nested algorithms
--
-- config_R_LEVEL ::= integer 
--       The number of nesting levels (1 => no nesting level)
--
-- config_GS_LEVEL ::= integer
--       The searching width 
--      (1 <= GS level <= the length of the longest RHS of production rules)
--
-- config_DEBUG ::= True | False
--       Printing debugging information
--
-- config_DISPLAY ::= True | False
--       No display of candidates for False
--
-- config_PRESENTATION ::= integer
--       print as it is for 0
--       ??? for 1
--
-- config_ALGORITHM ::= integer
--       use 3, the other algorithms are obsolete
--
-- config_COLLECT ::= True | False
--       collects candidates and frequencies from sample complete source programs
--
-- Usages: 
--    1) For parsing mode, config_COLLECT = False. The other configurations are ignored.
--    2) For sampling mode, config_COLLECT = True. The other configurations are ignored.
--    3-1) For server mode with online computation, use config_SIMPLE ~ config_ALGORITHM. 
--         config_COLLECT is ignored.
--    3-2) For server mode with looking up, use config_COLLECT = True.   
--
-- Programming:
--    1) For parsing mode, lexing and parsing are main functions.
--    2) For sampling mode, lexing and parsing are main functions with config_COLLECT = True.
--    3-1) For server mode with online computation, emacsServer and computeCandidate are main functions.
--    3-2) For server mode with looking up, emacsServer with config_TABSTATE = True
--
-- A typical example for server mode:
-- 
-- Configuration {
--   config_SIMPLE   = True,
--   config_R_LEVEL  = 1,
--   config_GS_LEVEL = 9, 
--   config_DEBUG    = False, 
--   config_DISPLAY  = False,
--   config_PRESENTATION = 0,
--   config_ALGORITHM = 3,
--   config_COLLECT = False
-- }
--


data Configuration =
  Configuration {
    config_SIMPLE :: Bool,      -- Simple(True), Nested(False)
    config_R_LEVEL :: Int,      -- 
    config_GS_LEVEL :: Int,     -- 
    config_DEBUG :: Bool,       -- True => Debugging on, False => Debugging off
    config_DISPLAY :: Bool,     -- True => Display in Emacs, False => Do not display in Emacs
    config_PRESENTATION :: Int, -- 0 : default, 1 : ...
    config_ALGORITHM :: Int,    -- 0 : BU,  1 : TD,  2 : PEPM [, 3 : BUTree ] 
    config_COLLECT :: Bool,     -- True => Collect parsing processes, False => No analysis
    config_TABSTATE :: Bool
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

writeConfig :: Configuration -> IO ()
writeConfig config =
  writeFile configFileName (show config)

         
         
