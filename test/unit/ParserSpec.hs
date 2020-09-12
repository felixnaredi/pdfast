{-# LANGUAGE OverloadedStrings #-}

module ParserSpec
  ( objectParseSpecs
  )
where

import           Data.ByteString                ( ByteString )
import           Data.Either                    ( isLeft )
import           Data.PDF.Object

import           Test.Hspec
import           Text.Parsec                    ( parse )

objectParseSpecs :: Spec
objectParseSpecs = do
  describe "Parses valid strings to simple objects" $ do
    parseNullObjects
    parseBooleanObjects
    parseNumericInts
    parseNumericReals
  describe "Parses malformed strings that could be objects" $ do
    malformed "True"
    malformed "fals"
    malformedNumbers

wellformed :: Object -> ByteString -> Spec
wellformed obj s =
  it ("Parsing " ++ show s ++ " to an object") $ case parse objectP "" s of
    Right res -> res `shouldBe` obj

parseNullObjects :: Spec
parseNullObjects = wellformed Null "null"

parseBooleanObjects :: Spec
parseBooleanObjects = do
  wellformed (Boolean True)  "true"
  wellformed (Boolean False) "false"

parseNumericInts :: Spec
parseNumericInts = do
  wellformed (NumericInt 123)   "123"
  wellformed (NumericInt 43445) "43445"
  wellformed (NumericInt 17)    "+17"
  wellformed (NumericInt (-98)) "-98"
  wellformed (NumericInt 0)     "0"

parseNumericReals :: Spec
parseNumericReals = do
  wellformed (NumericReal 34.5)     "34.5"
  wellformed (NumericReal (-3.62))  "-3.62"
  wellformed (NumericReal 123.6)    "+123.6"
  wellformed (NumericReal 4.0)      "4."
  wellformed (NumericReal (-0.002)) "-.002"
  wellformed (NumericReal 0.0)      "0.0"

malformed :: ByteString -> Spec
malformed s =
  it ("Parsing " ++ show s ++ " should fail")
    $          isLeft (parse objectP "" s)
    `shouldBe` True

malformedNumbers :: Spec
malformedNumbers = do
  malformed "+"
  malformed "."
  malformed "1.2.3"
  malformed "1..23"
  malformed "--1"
