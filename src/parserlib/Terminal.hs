{-# LANGUAGE GADTs #-}
module Terminal(Terminal(..), terminalToString) where

import TokenInterface

type Line   = Int
type Column = Int

data Terminal token where
  Terminal :: TokenInterface token => String -> Line -> Column -> token -> Terminal token

terminalToString :: TokenInterface token => Terminal token -> String
terminalToString (Terminal text line col tok) =
  text ++ " at (" ++ show line ++ ", " ++ show col ++ "): " ++ fromToken tok
