module Attrs where


-- | Associativity

data Associativity = Left | Right | Nonassoc
   deriving (Show, Read, Eq)

-- Production rules
type RuleNumber = Int
type Precedence = Int

data ProdRuleAttrs = ProdRuleAttrs [(RuleNumber, (Associativity, Precedence))]
   deriving (Show, Read)

-- Tokens or placeholders
type TokenOrPlaceholder = String

data TokenAttrs = TokenAttrs [(TokenOrPlaceholder, (Associativity, Precedence))]
   deriving (Show, Read)
