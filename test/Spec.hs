module Main (main) where

import Test.Hspec
import System.Path.WildMatch (wildCheckCase)
import Lib (ignore) -- Import the function from the new module

main :: IO ()
main = hspec $ do
  describe "wildCheckCase" $ do
    it "matches exact filenames" $ do
      wildCheckCase "tests.js" "tests.js" `shouldBe` True
      wildCheckCase "log.js" "log.js" `shouldBe` True
      wildCheckCase "tests.js" "test.js" `shouldBe` False

    it "matches wildcard patterns" $ do
      wildCheckCase "*.js" "add_test.js" `shouldBe` True
      wildCheckCase "temp*" "tempfile" `shouldBe` True
      wildCheckCase "tt*" "tt_file" `shouldBe` True

    it "does not match files that shouldn't be ignored" $ do
      wildCheckCase "*.json" "config.yaml" `shouldBe` False
      wildCheckCase "*.js" "config.json" `shouldBe` False

    it "handles case sensitivity" $ do
      wildCheckCase "*.JS" "add_test.js" `shouldBe` False

  describe "ignore function" $ do
    it "ignores files based on patterns" $ do
      let patterns = [".*", "*.json", "tests.js", "log.js", "tt*", "temp*", "meta*", "dev_*"]
      ignore "tests.js" patterns `shouldBe` True
      ignore "log.js" patterns `shouldBe` True
      ignore "add_test.js" patterns `shouldBe` False
      ignore "tempfile" patterns `shouldBe` True
      ignore "meta123" patterns `shouldBe` True
      ignore "dev_file" patterns `shouldBe` True
      ignore "random.txt" patterns `shouldBe` False
