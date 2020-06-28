module SynCompInterface where

data EmacsDataItem =
    LexError
  | ParseError [String]
  | SuccessfullyParsed
  | Candidate String
  deriving Show
