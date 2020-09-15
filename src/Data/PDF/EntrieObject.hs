module Data.PDF.EntrieObject
  ( EntrieObj(..)
  , EntrieNameValue
  , entrieObj
  )
where

import           Data.Bifunctor
import           Data.ByteString                ( ByteString )
import           Data.Char
import           Data.Maybe
import           Data.Word                      ( Word8 )
import qualified Data.ByteString               as B
import           Text.Parsec

-- | EntrieObj is an object that is a valid entrie to a dictionary.
data EntrieObj n = Null
                 | Boolean Bool
                 | NumericInt Int
                 | NumericReal Double
                 | LiteralStr ByteString
                 | HexadecStr [Word8]
                 | Name n
                 | IndirectRef Int Int
                 | Array [EntrieObj n]
                 | Dictionary [(n, EntrieObj n)]
  deriving (Eq, Show)

type EntrieObjParser n u m = ParsecT ByteString u m (EntrieObj n)
type EntrieNameValue = [Word8]

-- -----------------------------------------------------------------------------
-- Parsing
-- -----------------------------------------------------------------------------

-- | Parser for PDF objects.
entrieObj :: Monad m => EntrieObjParser EntrieNameValue u m
entrieObj =
  nullParser
    <|> boolean
    <|> name
    <|> ltgt
    <|> literalStr
    <|> array
    <|> try indirectRef
    <|> numeric

-- | Parser for null object.
nullParser :: Monad m => EntrieObjParser n u m
nullParser = string "null" >> return Null

-- | Parser for boolean objects.
boolean :: Monad m => EntrieObjParser n u m
boolean =
  (string "true" >> return (Boolean True))
    <|> (string "false" >> return (Boolean False))

-- | @numeric@ consumes a decimal number and the result is either an @Int@ or a 
-- @Double@ depending on whether or not the number contains a decimal sign.
--
-- If 'd1' and 'd2' are two string of digits ranging from '0' to '9' with no 
-- trailing or leading zeros then the following inputs will be succesfully 
-- parsed:
--
-- >  "d1"                  -- Evaluates as '(Left Int)'
-- >  "d1.", ".d1", "d1.d2" -- Evaluates as '(Right Double)'
-- >
--
-- In addition to this all inputs may begin with the characters plus '+' or 
-- minus '-'. Unless the numbers begins with a minus character the result will
-- be positive.
--
-- __NOTE__: Not sure if numbers must be followed by spaces. Since I beleive 
-- that the array "[1.2.3 ]" should not be interpreted as "[1.2 .3 ]" I have for 
-- the time being disallowed valid inputs to be followed by a decimal sign.
numeric :: Monad m => EntrieObjParser n u m
numeric = liftNum <$> (plusNumeric <|> minusNumeric <|> numericLiteral)
  where liftNum = either NumericInt NumericReal

plusNumeric :: Monad m => ParsecT ByteString u m (Either Int Double)
plusNumeric = char '+' >> numericLiteral

minusNumeric :: Monad m => ParsecT ByteString u m (Either Int Double)
minusNumeric = char '-' >> bimap negate negate <$> numericLiteral

numericLiteral :: Monad m => ParsecT ByteString u m (Either Int Double)
numericLiteral =
  (   (   intLiteral
      >>= \x -> (Right . (fromIntegral x +) <$> decimalDot) <|> return (Left x)
      )
    <|> Right
    <$> decimalDot1
    )
    <* notFollowedBy (char '.')
 where
  decimalDot1 = char '.' >> (decl <$> many1 digit)
  decimalDot  = char '.' >> (decl <$> many digit)
  decl [] = 0
  decl s  = fromIntegral (read s) / (10 ** fromIntegral (length s))

intLiteral :: (Monad m, Read a, Num a) => ParsecT ByteString u m a
intLiteral = read <$> many1 digit

byte :: Char -> Word8
byte = fromIntegral . ord

readOctal :: String -> Word8
readOctal = fromIntegral . foldl (\x c -> x * 8 + digitToInt c) 0

consA :: Applicative f => a -> f [a] -> f [a]
consA x = (<$>) (x :)

manyLimit :: Monad m => Int -> ParsecT s u m a -> ParsecT s u m [a]
manyLimit 0 _ = return []
manyLimit n p = (p >>= flip consA (manyLimit (n - 1) p)) <|> return []

-- | Parser for literal string objects.
literalStr :: Monad m => EntrieObjParser n u m
literalStr = LiteralStr <$> literal'
 where
  literal' = between (char '(') (char ')') next
  paren    = B.cons 40 <$> (B.append <$> literal' <*> (B.cons 41 <$> next))

  next     = escape <|> paren <|> regular <|> return B.empty

  escape   = char '\\' >> (escseq <|> octal <|> (anyToken >> next))
  escseq   = foldl (<|>) parserZero $ map
    (\(c, b) -> char c >> B.cons b <$> next)
    [ ('n' , 10)
    , ('r' , 13)
    , ('t' , 9)
    , ('b' , 8)
    , ('f' , 12)
    , ('(' , 40)
    , (')' , 41)
    , ('\\', 92)
    ]
  octal   = B.cons . readOctal <$> digits <*> next
  digits  = octDigit >>= flip consA (manyLimit 2 octDigit)
  regular = B.cons . byte <$> satisfy (')' /=) <*> next

-- | Parser for name objects with internal atomic value represented by bytes.
--
-- __NOTE__: It is not clear from the standards but looking at some PDF files it
-- seems as '/' and '(' and '[' can be used as delimeters for names. These 
-- examples is from an actual document:
-- >
-- > <</Type/Pages/Count 2/Kids[ 3 0 R 18 0 R] >>
-- >
--
-- This was a key value pair inside a dictionary.
-- >
-- > /Lang(en-GB) 
-- >
name :: Monad m => EntrieObjParser [Word8] u m
name = Name <$> name'

-- | Turns conforming names into lists of @Word8@. It is not a part of the 
-- @name@ parser since dictionaries uses raw name values as keys.
name' :: Monad m => ParsecT ByteString u m [Word8]
name' = char '/' >> many (numsign <|> regular)
 where
  numsign = char '#' >> hexByte
  regular = byte <$> satisfy (`elem` filter (`notElem` "/([") ['!' .. '~'])

-- | Parser for hexadecimal strings and dictionaries. They have a combined 
-- parser since they are both bounded by lesser than '<' and greater than '>'
-- characters.
ltgt :: Monad m => EntrieObjParser EntrieNameValue u m
ltgt = char '<' >> (hexadecStr <|> dictionary)
 where
  hexadecStr = HexadecStr <$> manyTill hexByte (char '>')

  dictionary = Dictionary <$> between
    (char '<' >> spaces)
    (spaces >> string ">>")
    (many ((,) <$> (name' <* spaces) <*> (spaces *> entrieObj)))

-- | Consumes two hex digits and returns it as a byte.
hexByte :: Monad m => ParsecT ByteString u m Word8
hexByte = hexi >>= \n -> fromIntegral . (n * 16 +) <$> hexi
  where hexi = digitToInt <$> hexDigit

indirectRef :: Monad m => EntrieObjParser n u m
indirectRef =
  IndirectRef <$> (intLiteral <* char ' ') <*> (intLiteral <* string " R")

array :: Monad m => EntrieObjParser EntrieNameValue u m
array = Array <$> between (char '[' >> spaces)
                          (spaces >> char ']')
                          (many (entrieObj <* spaces))
