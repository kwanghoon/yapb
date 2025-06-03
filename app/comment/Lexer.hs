module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

import qualified Control.Monad.Trans.State.Lazy as ST

mkFn :: Token -> LexAction Token IO () -- (String -> Maybe Token)
mkFn tok = \text -> return (Just tok)

skip :: LexAction Token IO ()          -- String -> Maybe Token
skip = \text -> return Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList = 
      [ ("/\\*", multiLineCommentBegin),
        ("//" ++ zeroOrMore "[^\n]" ++ "[\n]", skip ), -- rewritten // as this
        ("[ \t\n]", skip),
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

-- Regex writer utilities
zeroOrMore x = paren x ++ "*"     -- "(" ++ x ++ ")*"

paren x = "(" ++ x ++ ")"

multiLineCommentBegin :: LexAction Token IO ()          -- String -> Maybe Token
multiLineCommentBegin = \text0 -> -- /*
  --trace ("multiLineCommentBegin" ++ text0) $
    do  (state_parm_, line, col, text) <- ST.get
        let (newLine, newCol, newText) = mlc (tail (tail text)) line (col+2)
        -- lift $ putStrLn text0
        -- lift $ putStrLn (show line ++ ", " ++ show col ++ ", " ++ text)
        -- lift $ putStrLn (show newLine ++ ", " ++ show newCol ++ ", " ++ newText)
        ST.put (state_parm_, newLine, newCol, newText)
        return Nothing

  where
    mlc [] line col = (line, col, [])
    mlc ('*':'/':text) line col = (line, col+2, text)
    mlc ('\n':text) line col = mlc text (line+1) col
    mlc ('\r':text) line col = mlc text (line+1) col
    mlc (_:text) line col = mlc text line (col+1)

