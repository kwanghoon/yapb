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
        ("\\/\\/[^\n]*\n" , skip),
        ("\\("    , mkFn OPEN_PAREN_TOKEN),
        ("\\)"    , mkFn CLOSE_PAREN_TOKEN),
        ("\\{"    , mkFn OPEN_BRACE_TOKEN),
        ("\\}"    , mkFn CLOSE_BRACE_TOKEN),
        ("\\["    , mkFn OPEN_BRACKET_TOKEN),
        ("\\]"    , mkFn CLOSE_BRACKET_TOKEN),
        ("-[a-zA-Z][a-zA-Z0-9]*->", mkFn LOCFUN_TOKEN),
        ("\\."    , mkFn DOT_TOKEN),
        ("\\,"    , mkFn COMMA_TOKEN),
        ("\\;"    , mkFn SEMICOLON_TOKEN),
        ("\\:="   , mkFn ASSIGN_TOKEN),
        ("\\:"    , mkFn COLON_TOKEN),
        ("=="     , mkFn EQUAL_TOKEN),
        ("=>"     , mkFn ALT_ARROW_TOKEN),
        ("="      , mkFn DEF_TOKEN),
        ("\\|"    , mkFn BAR_TOKEN),
        ("\\\\"   , mkFn BACKSLASH_TOKEN),
        ("\\@"    , mkFn AT_TOKEN),
        ("!="     , mkFn NOTEQUAL_TOKEN),
        ("!"      , mkFn NOT_TOKEN),
        ("<="     , mkFn LESSEQUAL_TOKEN),
        ("<"      , mkFn LESSTHAN_TOKEN),
        (">="     , mkFn GREATEREQUAL_TOKEN),
        (">"      , mkFn GREATERTHAN_TOKEN),
        ("\\+"    , mkFn ADD_TOKEN),
        ("-"      , mkFn SUB_TOKEN),
        ("\\*"    , mkFn MUL_TOKEN),
        ("\\/"     , mkFn DIV_TOKEN),
        ("[0-9]+" , mkFn INTEGER_TOKEN),
        -- ("Unit"   , mkFn UNIT_TYPE_TOKEN),
        -- ("Int"    , mkFn INTEGER_TYPE_TOKEN),
        -- ("Bool"   , mkFn BOOLEAN_TYPE_TOKEN),
        -- ("String" , mkFn STRING_TYPE_TOKEN),    
        ("(True|False)" , mkFn BOOLEAN_TOKEN),
        ("\"[^\"]*\"" , mkFn STRING_TOKEN),   
        ("data"    , mkFn KEYWORD_DATA_TOKEN),
        ("let"     , mkFn KEYWORD_LET_TOKEN),
        ("end"     , mkFn KEYWORD_END_TOKEN),
        ("if"      , mkFn KEYWORD_IF_TOKEN),
        ("then"    , mkFn KEYWORD_THEN_TOKEN),
        ("else"    , mkFn KEYWORD_ELSE_TOKEN),
        ("case"    , mkFn KEYWORD_CASE_TOKEN),
        ("or"      , mkFn KEYWORD_OR_TOKEN),
        ("and"     , mkFn KEYWORD_AND_TOKEN),
        ("[a-zA-Z_][a-zA-Z0-9_]*"    , mkFn IDENTIFIER_TOKEN)
      ]
  } 
