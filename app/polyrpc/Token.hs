module Token where

import Prelude hiding(EQ)
import TokenInterface

data Token =
    END_OF_TOKEN
  | OPEN_PAREN_TOKEN
  | CLOSE_PAREN_TOKEN
  | OPEN_BRACE_TOKEN
  | CLOSE_BRACE_TOKEN
  | OPEN_BRACKET_TOKEN
  | CLOSE_BRACKET_TOKEN
  | IDENTIFIER_TOKEN
  | LOCFUN_TOKEN
  | DOT_TOKEN
  | COMMA_TOKEN
  | SEMICOLON_TOKEN
  | COLON_TOKEN
  | DEF_TOKEN         -- =
  | BAR_TOKEN
  | BACKSLASH_TOKEN
  | KEYWORD_DATA_TOKEN
  | KEYWORD_LET_TOKEN
  | KEYWORD_END_TOKEN
  | KEYWORD_IF_TOKEN
  | KEYWORD_THEN_TOKEN
  | KEYWORD_ELSE_TOKEN
  | KEYWORD_CASE_TOKEN
  | KEYWORD_OR_TOKEN
  | KEYWORD_AND_TOKEN
  | AT_TOKEN
  | ALT_ARROW_TOKEN
  | NOT_TOKEN
  | NOTEQUAL_TOKEN
  | EQUAL_TOKEN      -- ==
  | LESSTHAN_TOKEN
  | LESSEQUAL_TOKEN
  | GREATERTHAN_TOKEN
  | GREATEREQUAL_TOKEN
  | ADD_TOKEN
  | SUB_TOKEN
  | MUL_TOKEN
  | DIV_TOKEN
  | ASSIGN_TOKEN

  | INTEGER_TOKEN
  | BOOLEAN_TOKEN
  | STRING_TOKEN
  
  -- | UNIT_TYPE_TOKEN
  -- | INTEGER_TYPE_TOKEN
  -- | BOOLEAN_TYPE_TOKEN
  -- | STRING_TYPE_TOKEN
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKEN, "$"),
    (OPEN_PAREN_TOKEN, "("),
    (CLOSE_PAREN_TOKEN, ")"),
    (OPEN_BRACE_TOKEN, "{"),
    (CLOSE_BRACE_TOKEN, "}"),
    (OPEN_BRACKET_TOKEN, "["),
    (CLOSE_BRACKET_TOKEN, "]"),
    (IDENTIFIER_TOKEN, "identifier"),
    (LOCFUN_TOKEN, "LocFun"),
    (DOT_TOKEN, "."),
    (COMMA_TOKEN, ","),
    (SEMICOLON_TOKEN, ";"),
    (COLON_TOKEN, ":"),
    (DEF_TOKEN, "="),
    (BAR_TOKEN, "|"),
    (BACKSLASH_TOKEN, "\\"),
    (KEYWORD_DATA_TOKEN, "data"),
    (KEYWORD_LET_TOKEN, "let"),
    (KEYWORD_END_TOKEN, "end"),
    (KEYWORD_IF_TOKEN, "if"),
    (KEYWORD_THEN_TOKEN, "then"),
    (KEYWORD_ELSE_TOKEN, "else"),
    (KEYWORD_CASE_TOKEN, "case"),
    (KEYWORD_OR_TOKEN, "or"),
    (KEYWORD_AND_TOKEN, "and"),
    (AT_TOKEN, "@"),
    (ALT_ARROW_TOKEN, "=>"),
    (NOT_TOKEN, "!"),
    (NOTEQUAL_TOKEN, "!="),
    (EQUAL_TOKEN, "=="),
    (LESSTHAN_TOKEN, "<"),
    (LESSEQUAL_TOKEN, "<="),
    (GREATERTHAN_TOKEN, ">"),
    (GREATEREQUAL_TOKEN, ">="),
    (ADD_TOKEN, "+"),
    (SUB_TOKEN, "-"),
    (MUL_TOKEN, "*"),
    (DIV_TOKEN, "/"),
    (ASSIGN_TOKEN, ":="),
    (INTEGER_TOKEN, "integer"),
    (BOOLEAN_TOKEN, "boolean"),
    (STRING_TOKEN, "string")
    -- (UNIT_TYPE_TOKEN, "Unit"),
    -- (INTEGER_TYPE_TOKEN, "Int"),
    -- (BOOLEAN_TYPE_TOKEN, "Bool"),
    -- (STRING_TYPE_TOKEN, "String")
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
  
