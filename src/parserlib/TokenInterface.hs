module TokenInterface where

class TokenInterface token where
  toToken    :: String -> token
  fromToken  :: token -> String


  

  
