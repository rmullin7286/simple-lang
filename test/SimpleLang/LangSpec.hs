module SimpleLang.LangSpec where

import Test.Hspec
import Text.Megaparsec
import Data.Either

import SimpleLang.Lang

spec = describe "SimpleLang.Lang" $ do
    it "can parse identifiers" $ do
        (simpleParse identifier "id135ng") `shouldBe` (Right $ Identifier "id135ng")
    it "can parse statements separated by newlines" $ do
        let output = (simpleParse (many $ line importStmt) "import a\nimport b\nimport c")
        (length <$> output) `shouldBe` (Right 3)
    it "can parse top level expressions" $ do
        let output = simpleParse (line topLevel) "type A = {a : String; b : String}"
        (isRight output) `shouldBe` True