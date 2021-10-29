module SyntaxCompletionSpec where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import System.IO (readFile)

spec = hspec $ do
  describe "syntax complection yapb/app/syntaxcompletion" $ do
    let ex1_sml = "let val add = fn x =>"
    
    it ("[ex1.sml:simple] " ++ ex1_sml) $ do
      results <- computeCand False ex1_sml "" True
      results `shouldBe` [Candidate "white ..."]

    it ("[ex1.sml:nested] " ++ ex1_sml) $ do
      results <- computeCand False ex1_sml "" False
      results `shouldBe` [Candidate "white ...",Candidate "white ... white in white ... white end"]

    let ex2_sml = "let val add = fn y"
    
    it ("[ex2.sml:simple] " ++ ex2_sml) $ do
      results <- computeCand False ex2_sml "" True
      results `shouldBe` [Candidate "white => white ..."]

    it ("[ex2.sml:nested] " ++ ex2_sml) $ do
      results <- computeCand False ex2_sml "" False
      results `shouldBe`
        [Candidate "white => white ...",Candidate "white => white ... white in white ... white end"]

    let test1_sml = "let val app = fn f => fn x => f x in let val add = fn y => y in"
    let test1_sml_end = "  end"
    
    it ("[test1.sml:simple] " ++ test1_sml ++ " [cursor] " ++ test1_sml_end) $ do
      results <- computeCand False test1_sml test1_sml_end True
      results `shouldBe` [Candidate "white ... gray end 1 66 "]

    it ("[test1.sml:nested] " ++ test1_sml ++ " [cursor] " ++ test1_sml_end) $ do
      results <- computeCand False test1_sml test1_sml_end False
      results `shouldBe`
        [Candidate "white ... gray end 1 66 ",Candidate "white ... gray end 1 66  white end"]

    let test1_sml = "let val app = fn f => fn x => f x in let val add = fn y => y in"
    let test1_sml_end = "  end !="
    
    it ("[test1.sml:simple:invalid tokens] " ++ test1_sml ++ " [cursor] " ++ test1_sml_end) $ do
      results <- computeCand False test1_sml test1_sml_end True
      results `shouldBe` [LexError]

    let test1_sml = "let val app = fn f => fn x => f x in let val add = fn y => y in"
    let test1_sml_end = "  end !="
    
    it ("[test1.sml:nested:invalid tokens] " ++ test1_sml ++ " [cursor] " ++ test1_sml_end) $ do
      results <- computeCand False test1_sml test1_sml_end False
      results `shouldBe` [LexError]

    let test2_sml = "fn x => f (f (f (f x"
    
    it ("[test2.sml:simple] " ++ test2_sml) $ do
      results <- computeCand False test2_sml "" True
      results `shouldBe` [Candidate "white )"]  -- for loosely simple mode
      -- results `shouldBe` []  -- for strictly simple mode

    it ("[test2.sml:nested] " ++ test2_sml) $ do
      results <- computeCand False test2_sml "" False
      results `shouldBe`
        [Candidate "white )",Candidate "white ) white )",Candidate "white ) white ) white )"]

    let test3_sml = "let val add = x "
    
    it ("[test3.sml:simple] " ++ test3_sml) $ do
      results <- computeCand False test3_sml "" True
      results `shouldBe` [Candidate "white in white ... white end"]  -- for loosely simple mode
      -- results `shouldBe` []  -- for strictly simple mode

    it ("[test3.sml:nested] " ++ test3_sml) $ do
      results <- computeCand False test3_sml "" False
      results `shouldBe` [Candidate "white in white ... white end"]

    let test4_sml = "fn x y => "
    
    it ("[test4.sml:simple] " ++ test4_sml) $ do
      results <- computeCand False test4_sml "" True
      results `shouldBe`
        [ParseError ["y at (1, 6): identifier","=> at (1, 8): =>","$ at (1, 11): $"]]

    it ("[test4.sml:nested] " ++ test4_sml) $ do
      results <- computeCand False test4_sml "" False
      results `shouldBe`
        [ParseError ["y at (1, 6): identifier","=> at (1, 8): =>","$ at (1, 11): $"]]

    let lexerror = "x != y "
    
    it ("[lex error:simple] " ++ lexerror) $ do
      results <- computeCand False lexerror "" True
      results `shouldBe` [LexError]

    it ("[lex error:nested] " ++ lexerror) $ do
      results <- computeCand False lexerror "" False
      results `shouldBe` [LexError]
