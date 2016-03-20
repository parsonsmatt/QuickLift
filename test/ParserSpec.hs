{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ParserSpec where

import Data.Scientific
import Test.Hspec
import Text.QuasiText
import Data.Either
import Parser

spec :: Spec
spec = do
    describe "liftName" $ do
        let p = parse liftName ""
        it "parses a lift name" $ do
            p "Squat:" `shouldBe` Right "Squat"
            p "    Squat:  \n" `shouldBe` Right "Squat"
        it "requires a ':' to indicate end of name" $ do
            p "Squat" `shouldSatisfy` isLeft
    describe "setLine" $ do
        let p = parse setLine ""
        it "parses a set line only weight" $ do
            p "100" `shouldBe`
                Right [Set 100 1]
        it "parses a set with reps" $ do
            p "100 x 10" `shouldBe`
                Right [Set 100 10]
        it "repeats a set" $ do
            p "100 x 10 x 2" `shouldBe`
                Right (replicate 2 (Set 100 10))
        it "can work with commas" $ do
            p "100 x 10, 5" `shouldBe`
                Right [Set 100 10, Set 100 5]
        it "can work with commas and sets" $ do
            p "100 x 10 x 2, 5" `shouldBe`
                Right [Set 100 10, Set 100 10, Set 100 5]
            p "100 x 10 x 2, 5 x 2" `shouldBe`
                Right [Set 100 10, Set 100 10, Set 100 5, Set 100 5]
    describe "decimal" $ do
        let p = parse decimal ""
        it "parses ints" $ do
            p "10" `shouldBe` Right (scientific 10 0)
        it "parses floats" $ do
            p "10.5" `shouldBe` Right (scientific 105 (-1))
            p "10.25" `shouldBe` Right (scientific 1025 (-2))
    describe "xThenInt" $ do
        let p = parse xThenInt ""
        it "Skips an optional x" $ do
            p "10" `shouldBe` Right 10
        it "succeeds with an x" $ do
            p "x10" `shouldBe` Right 10
        it "doesn't care about spaces" $ do
            p " x 10" `shouldBe` Right 10
        it "defaults to 1" $ do
            p "" `shouldBe` Right 1
    describe "repsxsets" $ do
        let p = parse repsxsets ""
        it "parses full format correctly" $ do
            p "10 x 10" `shouldBe` Right (10, 10)
        it "parses missing sets correctly" $ do
            p "10" `shouldBe` Right (10, 1)
        it "parses blank as (1, 1)" $ do
            p "" `shouldBe` Right (1, 1)

    describe "liftSets" $ do
        let p = parse liftSets ""
        it "parses many lift sets" $ do
            let s = [embed|
10
10 x 2
10 x 2 x 3
|]
            p s `shouldBe`
                Right [Set 10 1, Set 10 2, Set 10 2, Set 10 2, Set 10 2]

    describe "session" $ do
        let p = parse session ""
            ex =  Lift "Squat" [ Set 100 1, Set 110 2, Set 115 3,
                               Set 115 3, Set 115 3, Set 100 8, Set 100 5, Set
                               102.5 3, Set 102.5 3, Set 102.5 3, Set 102.5 5]
        it "parses an empty lift" $ do
            p "" `shouldBe` Right (Session [])
        it "parses a single set" $ do
            p "Squat: 100" `shouldBe` Right (Session [Lift "Squat" [Set 100 1]])
        it "is good with a newline" $ do
            p "Squat:\n100" `shouldBe` Right (Session [Lift "Squat" [Set 100 1]])
        it "parses a single lift" $ do
            let s = [embed|
Squat:
100
110 x 2
115 x 3 x 3
100 x 8, 5
102.5 x 3 x 3, 5

|]
            p s `shouldBe` Right (Session [ex])
        it "parses two lifts" $ do
            let s = [embed|
Squat:
100

Squat:
100
|]
            p s `shouldBe` Right (Session (replicate 2 (Lift "Squat" [Set 100 1])))
