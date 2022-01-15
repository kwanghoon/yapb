module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

mkFn :: Token -> LexAction Token IO () -- (String -> Maybe Token)
mkFn tok = \text -> return (Just tok)

skip :: LexAction Token IO ()          -- String -> Maybe Token
skip = \text -> return Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList = 
      [ ("[ \t\n]", skip),
        ("[0-9]+" , mkFn INTEGER_NUMBER),
        ("\\("    , mkFn OPEN_PAREN),
        ("\\)"    , mkFn CLOSE_PAREN),
        ("\\+"    , mkFn ADD),
        ("\\-"    , mkFn SUB),
        ("\\*"    , mkFn MUL),
        ("\\/"    , mkFn DIV),
        ("\\="    , mkFn EQ),
        ("\\;"    , mkFn SEMICOLON),
        ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
      ]
  } 
