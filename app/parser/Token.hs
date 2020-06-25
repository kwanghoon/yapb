module Token(Token(..)) where

import Prelude hiding(EQ)
import TokenInterface

data Token =
    END_OF_TOKEN
  | OPEN_PAREN  | CLOSE_PAREN
  | IDENTIFIER  | INTEGER_NUMBER
  | ADD  | SUB  | MUL  | DIV
  | EQ  | SEMICOLON
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKEN, "$"),
    (OPEN_PAREN, "("), (CLOSE_PAREN, ")"),
    (IDENTIFIER, "identifier"), (INTEGER_NUMBER, "integer_number"),
    (ADD, "+"), (SUB, "-"), (MUL, "*"), (DIV, "/"),
    (EQ, "="), (SEMICOLON, ";")  
  ]

findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

findStr str [] = Nothing
findStr str ((tok,str_):list)
  | str == str_ = Just tok
  | otherwise   = findStr str list

instance TokenInterface Token where
  toToken str   =
    case findStr str tokenStrList of
      Nothing  -> error ("toToken: " ++ str)
      Just tok -> tok
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
  

