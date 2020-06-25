module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

mkFn :: Token -> (String -> Maybe Token)
mkFn tok = \text -> Just tok

skip :: String -> Maybe Token
skip = \text -> Nothing

lexerSpec :: LexerSpec Token
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList = 
      [ ("[ \t\n]", skip),
        ("\\("    , mkFn OPEN_PAREN),
        ("\\)"    , mkFn CLOSE_PAREN),
        ("fn"    , mkFn FN),
        ("let"    , mkFn LET),
        ("in"    , mkFn IN),
        ("end"    , mkFn END),
        ("val"    , mkFn VAL),
        ("=>"    , mkFn ARROW),
        ("="    , mkFn EQ),
        ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
      ]
  } 
