module SynCompInterface (EmacsDataItem(..)) where

data EmacsDataItem =
    LexError              -- Lex error at some terminal (not $)
  | ParseError [String]   -- Parse error at some terminal (not $)
  | Candidate String      -- Parse error at the cursor position returning a candidate string 
  | SuccessfullyParsed    -- Successfully parsed until the cursor position
  deriving Show


