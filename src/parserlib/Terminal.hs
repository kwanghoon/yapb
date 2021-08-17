{-# LANGUAGE GADTs #-}
module Terminal(Terminal(..), terminalToString, terminalToLine, terminalToCol, token_na) where

import TokenInterface

import Data.Maybe

type Line   = Int
type Column = Int

data Terminal token where  -- Todo: data Terminal token vs. data Symbol = ... | Terminal String in CFG.hs ??
  Terminal :: TokenInterface token => String -> Line -> Column -> Maybe token -> Terminal token
  -- Todo: In Maybe token, Just token for parsing, and Nothing is for syntax complection!

token_na = "token n/a"

terminalToString :: TokenInterface token => Terminal token -> String
terminalToString (Terminal text line col (Just tok)) =
  text ++ " at (" ++ show line ++ ", " ++ show col ++ "): " ++ fromToken tok

terminalToString (Terminal text line col Nothing) =
  text ++ " at (" ++ show line ++ ", " ++ show col ++ "): " ++ token_na

terminalToLine :: TokenInterface token => Terminal token -> Int
terminalToLine (Terminal text line col tok) = line

terminalToCol :: TokenInterface token => Terminal token -> Int
terminalToCol (Terminal text line col tok) = col
