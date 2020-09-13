{-# LANGUAGE LambdaCase #-}

module Data.PDF.Object
  ( Object(..)
  , NameValue
  , objectP
  )
where

import           Data.ByteString                ( ByteString )
import           Data.Char                      ( digitToInt )
import           Data.Word                      ( Word8 )
import qualified Data.ByteString               as B
import           Data.Map.Strict                ( Map )
import           Text.Parsec

type NameValue = [Word8]

data Object n = Null
               | Boolean Bool
               | NumericInt Int
               | NumericReal Double
               | LiteralStr ByteString
               | HexadecStr [Word8]
               | Name n
               | Array [Object n]
               | Dictionary (Map n (Object n))
               | NumberTree (Map Int (Object n))
               | Stream
               | Indirect
  deriving (Eq, Show)

type ObjectParser n u m = ParsecT ByteString u m (Object n)

-- | Parser for PDF objects.
objectP :: Monad m => ObjectParser NameValue u m
objectP = nullP <|> booleanP <|> numericP <|> nameP <|> hexadecStrP

-- | Parser for null object.
nullP :: Monad m => ObjectParser n u m
nullP = string "null" >> return Null

-- | Parser for boolean objects.
booleanP :: Monad m => ObjectParser n u m
booleanP =
  (string "true" >> return (Boolean True))
    <|> (string "false" >> return (Boolean False))

-- | Parser for numerical objects.
numericP :: Monad m => ObjectParser n u m
numericP = (char '-' >> negate' <$> num) <|> (char '+' >> num) <|> num
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

-- | Parser for hexadecimal strings objects.
hexadecStrP :: Monad m => ObjectParser n u m
hexadecStrP = between (char '<') (char '>') (HexadecStr <$> many hexByte)

-- | Consumes two hex digits and returns it as a byte.
hexByte :: Monad m => ParsecT ByteString u m Word8
hexByte = hexi >>= \n -> hexi >>= return . fromIntegral . (n * 16 +)
  where hexi = digitToInt <$> hexDigit

-- | Parser for name objects with internal atomic value represented by bytes.
nameP :: Monad m => ObjectParser [Word8] u m
nameP = char '/' >> Name <$> many1 (numsign <|> regular)
 where
  numsign = char '#' >> hexByte
  regular = fromIntegral . fromEnum <$> oneOf ['!' .. '~']
