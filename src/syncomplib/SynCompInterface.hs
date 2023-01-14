module SynCompInterface (EmacsDataItem(..), stateEmacsDataItem) where

data EmacsDataItem =
    LexError              -- Lex error at some terminal (not $)
  | ParseError String     -- Parse error at the text (not $)
  | SuccessfullyParsed    -- Successfully parsed until the cursor position

  -- config_COLLECT = False
  | Candidate String      -- Parse error at the cursor position returning a candidate string

  -- config_COLLECT = True
  | ParsingStateAtTabPosition Int  -- Parsing states at the encouter of parse errors

  deriving (Eq, Show)

stateEmacsDataItem :: Int -> EmacsDataItem
stateEmacsDataItem = ParsingStateAtTabPosition