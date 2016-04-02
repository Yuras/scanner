{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import Scanner

import Prelude hiding (take, takeWhile)
import Data.Either
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Test.Hspec

main :: IO ()
main = hspec $ do
  anyWord8Spec
  stringSpec
  takeSpec
  takeWhileSpec
  lookAheadSpec

anyWord8Spec :: Spec
anyWord8Spec = describe "anyWord8" $ do
  it "should return the current byte" $ do
    let bs = ByteString.pack [42, 43]
    scanOnly anyWord8 bs `shouldBe` Right 42

  it "should consume the current byte" $ do
    let bs = ByteString.pack [42, 43]
    scanOnly (anyWord8 *> anyWord8) bs `shouldBe` Right 43

    let bs' = Lazy.ByteString.fromChunks
          [ ByteString.pack [42]
          , ByteString.pack [43]
          , ByteString.pack [44]
          ]
    scanLazy (anyWord8 *> anyWord8 *> anyWord8) bs' `shouldBe` Right 44

  it "should ask for more input" $ do
    let bs = Lazy.ByteString.fromChunks
          [ ByteString.pack [42]
          , ByteString.pack [43]
          ]
    scanLazy (anyWord8 *> anyWord8) bs `shouldBe` Right 43

  it "should fail on end of input" $ do
    let bs = ByteString.empty
    scanOnly anyWord8 bs `shouldSatisfy` isLeft

stringSpec :: Spec
stringSpec = describe "string" $ do
  it "should consume the string" $ do
    let bs = "hello world"
    scanOnly (string "hello" *> anyWord8) bs `shouldBe` Right 32

  it "should ask for more input" $ do
    let bs = Lazy.ByteString.fromChunks
          [ "hel"
          , "lo"
          ]
    scanLazy (string "hello") bs `shouldBe` Right ()

  it "should fail on wrong input" $ do
    let bs = "helo world"
    scanOnly (string "hello") bs `shouldSatisfy` isLeft

takeSpec :: Spec
takeSpec = describe "take" $ do
  it "should return the first n bytes" $ do
    let bs = "hello world"
    scanOnly (take 5) bs `shouldBe` Right "hello"

  it "should ask for more input" $ do
    let bs = Lazy.ByteString.fromChunks
          [ "he"
          , "l"
          , "lo world"
          ]
    scanLazy (take 5) bs `shouldBe` Right "hello"

  it "should fail on end of input" $ do
    let bs = "hell"
    scanOnly (take 5) bs `shouldSatisfy` isLeft

    let bs' = Lazy.ByteString.fromChunks
          [ "he"
          , "l"
          , "l"
          ]
    scanLazy (take 5) bs' `shouldSatisfy` isLeft

takeWhileSpec :: Spec
takeWhileSpec = describe "takeWhile" $ do
  it "should return bytes according to the predicate" $ do
    let bs = "hello world"
    scanOnly (takeWhile (/= 32)) bs `shouldBe` Right "hello"

  it "should ask for more input" $ do
    let bs = Lazy.ByteString.fromChunks
          [ "he"
          , "l"
          , "lo world"
          ]
    scanLazy (takeWhile (/= 32)) bs `shouldBe` Right "hello"

  it "should return everything is predicate where becomes False" $ do
    let bs = "hello"
    scanOnly (takeWhile (/= 32)) bs `shouldBe` Right "hello"

lookAheadSpec :: Spec
lookAheadSpec = describe "lookAhead" $ do
  it "should return the next byte" $ do
    let bs = ByteString.pack [42, 43]
    scanOnly lookAhead bs `shouldBe` Right (Just 42)

  it "should return Nothing on end of input" $ do
    let bs = ByteString.empty
    scanOnly lookAhead bs `shouldBe` Right Nothing

  it "should not consume input" $ do
    let bs = ByteString.pack [42, 43]
    scanOnly (lookAhead *> anyWord8) bs `shouldBe` Right 42

  it "should ask for more input" $ do
    let bs = Lazy.ByteString.fromChunks
          [ ByteString.pack [42]
          , ByteString.pack [43]
          ]
    scanLazy (anyWord8 *> lookAhead) bs `shouldBe` Right (Just 43)
