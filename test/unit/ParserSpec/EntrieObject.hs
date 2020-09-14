{-# LANGUAGE OverloadedStrings #-}

module ParserSpec.EntrieObject
  ( entrieObjParseSpecs
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import           Data.Char
import           Data.Either                    ( isLeft )
import           Data.PDF.EntrieObject
import           Data.Word                      ( Word8 )
import           Test.Hspec
import           Text.Parsec                    ( parse )
import           Tools.PDF

-- -----------------------------------------------------------------------------
-- Test suits
-- -----------------------------------------------------------------------------

entrieObjParseSpecs :: Spec
entrieObjParseSpecs = do
  describe "Parses valid strings to simple objects" $ do
    parseNullObjects
    parseBooleanObjects
    parseNumericInts
    parseNumericReals
    parseLiteralStrings
    parseHexadecStrings
    parseNames
    parseArrays
  describe "Parses malformed strings that could be objects" $ do
    malformed "True"
    malformed "fals"
    malformedNumbers
    malformedLiteralStrings
    malformedHexadecStrings
    malformedNames

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

parseLiteralStrings :: Spec
parseLiteralStrings = mapM_
  (\(s, res) -> wellformed (LiteralStr res) s)
  [ ("(This is a string)", "This is a string")
  , ( "(Strings may contain newlines\nand such.)"
    , "Strings may contain newlines\nand such."
    )
  , ( "(Strings may contain balanced parentheses ())"
    , "Strings may contain balanced parentheses ()"
    )
  , ( "(special characters (*!&}^% and so on).)"
    , "special characters (*!&}^% and so on)."
    )
  , ("()"                     , "")
  , ("( w h i t e\tspace\n)"  , " w h i t e\tspace\n")
  , ("(\\n\\r\\t\\b\\f\\(\\))", "\n\r\t\b\f()")
  , ("(\\9NIN\\E)"            , "NIN")
  , ("(\\053)"                , "+")
  , ("(\\53)"                 , "+")
  , ("(\\053123)"             , "+123")
  , ("(\\0ZERO)"              , "\0ZERO")
  ]

parseHexadecStrings :: Spec
parseHexadecStrings = mapM_
  (\(s, res) -> wellformed (HexadecStr $ B.unpack res) s)
  [ ("<48656C6C6F20576F726C6421>"      , "Hello World!")
  , ("<230A796F0D4C4F2009537721345C67>", "#\nyo\rLO \tSw!4\\g")
  , ("<>"                              , "")
  , ("<6D497845644c6F576552614e645570506552>", "mIxEdLoWeRaNdUpPeR")
  ]

parseNames :: Spec
parseNames = mapM_
  (\(s, res) -> wellformed (nameObj res) s)
  [ ("/Name1"                  , "Name1")
  , ("/ASomewhatLongerName"    , "ASomewhatLongerName")
  , ("/"                       , "")
  , ("/A;Name_With-Various***Characters?", "A;Name_With-Various***Characters?")
  , ("/1.2"                    , "1.2")
  , ("/$$"                     , "$$")
  , ("/@pattern"               , "@pattern")
  , ("/null"                   , "null")
  , ("/Lime#20Green"           , "Lime Green")
  , ("/paired#28#29parentheses", "paired()parentheses")
  , ("/The_Key_of_F#23_Minor"  , "The_Key_of_F#_Minor")
  , ("/A#42"                   , "AB")
  , ("/Type/Pages"             , "Type")
  , ("/Lang(en-GB)"            , "Lang")
  , ("/Kids[ 3 0 R 18 0 R]"    , "Kids")
  ]

parseArrays :: Spec
parseArrays = mapM_
  (\(s, res) -> wellformed (Array res) s)
  [ ("[]"            , [])
  , ("[ 1 2 /hello ]", [numiObj 1, numiObj 2, nameObj "hello"])
  , ("[/A/2//C3 ]", [nameObj "A", nameObj "2", nameObj "", nameObj "C3"])
  , ( "[/Lang(en-GB)[123 45.6]7]"
    , [ nameObj "Lang"
      , litStrObj "en-GB"
      , Array [numiObj 123, numrObj 45.6]
      , numiObj 7
      ]
    )
  , ( "[\n\t true\n /space\r\n-.0 \r null\n\n]"
    , [true, nameObj "space", numrObj (-0.0), Null]
    )
  ]


malformedNumbers :: Spec
malformedNumbers = do
  malformed "+"
  malformed "."
  malformed "1.2.3"
  malformed "1..23"
  malformed "--1"

malformedLiteralStrings :: Spec
malformedLiteralStrings = do
  malformed "(missing ending parentheses"
  malformed "(unbalanced (literal)"
  malformed "(UnbAlAncEd (lItErAL) (* (* )"
  malformed "(this ( dummy -->\\) )"
  malformed "(Dummy Ending \\)"

malformedHexadecStrings :: Spec
malformedHexadecStrings = do
  malformed "<3C4E4F454E44212121"
  malformed "<737061 206365>"
  malformed "<72rrr7272525252>"

malformedNames :: Spec
malformedNames = do
  malformed "/f#ail"
  malformed "/bad-ending#"
  malformed "/BAD_END#6"

-- -----------------------------------------------------------------------------
-- Test makers
-- -----------------------------------------------------------------------------

-- | Test succeeds if the result from the parsed input is equal to the given 
-- object.
wellformed :: EntrieObj EntrieNameValue -> ByteString -> Spec
wellformed obj s =
  it ("Parsing " ++ show s ++ " to an object") $ case parse entrieObj "" s of
    Right res -> res `shouldBe` obj
    Left  err -> liftIO $ print err >> undefined

-- | Test succeeds if the parsing of the input results in an error.
malformed :: ByteString -> Spec
malformed s =
  it ("Parsing " ++ show s ++ " should fail")
    $          isLeft (parse entrieObj "" s)
    `shouldBe` True

-- -----------------------------------------------------------------------------
-- Object makers
-- -----------------------------------------------------------------------------

true :: EntrieObj n
true = Boolean True

false :: EntrieObj n
false = Boolean False

numiObj :: Integral a => a -> EntrieObj n
numiObj = NumericInt . fromIntegral

numrObj :: Double -> EntrieObj n
numrObj = NumericReal

nameObj :: ByteString -> EntrieObj EntrieNameValue
nameObj = Name . B.unpack

unpackStr :: String -> [Word8]
unpackStr = map (fromIntegral . ord)

packStr :: String -> ByteString
packStr = B.pack . unpackStr

litStrObj :: String -> EntrieObj n
litStrObj = LiteralStr . packStr

hexaStrObj :: String -> EntrieObj n
hexaStrObj = HexadecStr . unpackStr . strToHexadecStr
