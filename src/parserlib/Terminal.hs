{-# LANGUAGE GADTs #-}
module Terminal(Terminal(..), terminalToString, terminalToLine, terminalToCol) where

import TokenInterface

type Line   = Int
type Column = Int

data Terminal token where  -- Todo: data Terminal token vs. data Symbol = ... | Terminal String in CFG.hs ??
  Terminal :: TokenInterface token => String -> Line -> Column -> token -> Terminal token

terminalToString :: TokenInterface token => Terminal token -> String
terminalToString (Terminal text line col tok) =
  text ++ " at (" ++ show line ++ ", " ++ show col ++ "): " ++ fromToken tok

terminalToLine :: TokenInterface token => Terminal token -> Int
terminalToLine (Terminal text line col tok) = line

terminalToCol :: TokenInterface token => Terminal token -> Int
terminalToCol (Terminal text line col tok) = col