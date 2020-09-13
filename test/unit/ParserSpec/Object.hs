{-# LANGUAGE OverloadedStrings #-}

module ParserSpec.Object
  ( objectParseSpecs
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
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
    parseHexadecStrings
    parseNames
  describe "Parses malformed strings that could be objects" $ do
    malformed "True"
    malformed "fals"
    malformedNumbers
    malformedHexadecStrings
    malformedNames

wellformed :: Object NameValue -> ByteString -> Spec
wellformed obj s =
  it ("Parsing " ++ show s ++ " to an object") $ case parse objectP "" s of
    Right res -> res `shouldBe` obj
    Left  err -> liftIO $ print err >> undefined

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

parseHexadecStrings :: Spec
parseHexadecStrings = mapM_
  (\(x, s) -> wellformed (HexadecStr $ B.unpack x) s)
  [ ("Hello World!"       , "<48656C6C6F20576F726C6421>")
  , ("#\nyo\rLO \tSw!4\\g", "<230A796F0D4C4F2009537721345C67>")
  , (""                   , "<>")
  , ("mIxEdLoWeRaNdUpPeR", "<6D497845644c6F576552614e645570506552>")
  ]

parseNames :: Spec
parseNames = mapM_
  (\(v, s) -> wellformed (Name $ B.unpack v) s)
  [ ("Name1"              , "/Name1")
  , ("ASomewhatLongerName", "/ASomewhatLongerName")
  , ("A;Name_With-Various***Characters?", "/A;Name_With-Various***Characters?")
  , ("1.2"                , "/1.2")
  , ("$$"                 , "/$$")
  , ("@pattern"           , "/@pattern")
  , ("Lime Green"         , "/Lime#20Green")
  , ("paired()parentheses", "/paired#28#29parentheses")
  , ("The_Key_of_F#_Minor", "/The_Key_of_F#23_Minor")
  , ("AB"                 , "/A#42")
  ]

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

malformedHexadecStrings :: Spec
malformedHexadecStrings = do
  malformed "<3C4E4F454E44212121"
  malformed "<737061 206365>"
  malformed "<72rrr7272525252>"

malformedNames :: Spec
malformedNames = do
  malformed "/"
  malformed "/f#ail"
  malformed "/bad-ending#"
  malformed "/BAD_END#6"
