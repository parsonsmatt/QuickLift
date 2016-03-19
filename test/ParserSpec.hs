{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ParserSpec where

import Test.Hspec
-- import Text.QuasiText
import Data.Either
import Parser

skip :: String -> t -> Expectation
skip s _ = pendingWith s

spec :: Spec
spec = describe "Parser" $ do
    describe "liftName" $ do
        let p = parse liftName ""
        it "parses a lift name" $ do
            p "Squat:" `shouldBe` Right "Squat"
            p "    Squat:  \n" `shouldBe` Right "Squat"
        it "requires a ':' to indicate end of name" $ do
            p "Squat" `shouldSatisfy` isLeft
    describe "setLine" $ do
        let p = parse setLine ""
        it "parses a set line" $ do
            p "100 x 10" `shouldBe`
                Right [Set 100 10]
        it "repeats a set" $ do
            p "100 x 10 x 2" `shouldBe`
                Right (replicate 2 (Set 100 10))
        it "no reps assumes to be 1 rep" $ do
            p "100" `shouldBe`
                Right [Set 100 1]
        it "can work with commas" . skip "not implemented" $ do
            p "100 x 10, 5" `shouldBe`
                Right [Set 100 10, Set 100 5]
        it "can work with commas and sets" . skip "not implemented" $ do
            p "100 x 10 x 2, 5" `shouldBe`
                Right [Set 100 10, Set 100 10, Set 100 5]
            p "100 x 10 x 2, 5 x 2" `shouldBe`
                Right [Set 100 10, Set 100 10, Set 100 5, Set 100 5]
