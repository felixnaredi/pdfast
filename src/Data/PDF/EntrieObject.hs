{-# LANGUAGE LambdaCase #-}

module Data.PDF.EntrieObject
  ( EntrieObj(..)
  , entrieObj
  )
where

import           Data.ByteString                ( ByteString )
import           Data.Char
import           Data.Word                      ( Word8 )
import qualified Data.ByteString               as B
import           Data.Map.Strict                ( Map )
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
                 | Dictionary (Map n (EntrieObj n))
                 | NumberTree (Map Int (EntrieObj n))
  deriving (Eq, Show)

type EntrieObjParser n u m = ParsecT ByteString u m (EntrieObj n)

-- | Parser for PDF objects.
entrieObj :: Monad m => EntrieObjParser [Word8] u m
entrieObj = nullObj <|> boolean <|> name <|> numeric <|> hexadecStr <|> literal

-- | Parser for null object.
nullObj :: Monad m => EntrieObjParser n u m
nullObj = string "null" >> return Null

-- | Parser for boolean objects.
boolean :: Monad m => EntrieObjParser n u m
boolean =
  (string "true" >> return (Boolean True))
    <|> (string "false" >> return (Boolean False))

-- | Parser for numeric objects.
numeric :: Monad m => EntrieObjParser n u m
numeric = (char '-' >> negate' <$> num) <|> (char '+' >> num) <|> num
 where
  negate' (NumericInt  x) = NumericInt (negate x)
  negate' (NumericReal x) = NumericReal (negate x)

  num = sepBy digits (char '.') >>= \case
    [s@(_ : _)] -> return $ NumericInt $ read s
    [[], []]    -> parserZero <?> "number"
    [s1, s2]    -> return $ NumericReal (expos s1 + fracs s2)
    _           -> parserZero <?> "number"

  digits = many $ oneOf "0123456789"
  expos  = value (10 *)
  fracs s = value (/ 10) (reverse s) / 10
  value f = foldl (\acc x -> f acc + fromIntegral x) 0 . map digitToInt

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
literal :: Monad m => EntrieObjParser n u m
literal = LiteralStr <$> literal'
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

-- | Parser for hexadecimal string objects.
hexadecStr :: Monad m => EntrieObjParser n u m
hexadecStr = between (char '<') (char '>') (HexadecStr <$> many hexByte)

-- | Consumes two hex digits and returns it as a byte.
hexByte :: Monad m => ParsecT ByteString u m Word8
hexByte = hexi >>= \n -> fromIntegral . (n * 16 +) <$> hexi
  where hexi = digitToInt <$> hexDigit

-- | Parser for name objects with internal atomic value represented by bytes.
name :: Monad m => EntrieObjParser [Word8] u m
name = char '/' >> Name <$> many (numsign <|> regular)
 where
  numsign = char '#' >> hexByte
  regular = fromIntegral . fromEnum <$> oneOf ['!' .. '~']

