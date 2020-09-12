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

data Object = Null
            | Boolean Bool
            | NumericInt Int
            | NumericReal Double
            | LiteralStr ByteString
            | HexadecStr [Word8]
            | Name NameValue
            | Array [Object]
            | Dictionary (Map NameValue Object)
            | NumberTree (Map Int Object)
            | Stream
            | Indirect
  deriving (Eq, Show)

type ObjectParser u m = ParsecT ByteString u m Object

objectP :: Monad m => ObjectParser u m
objectP = nullP <|> booleanP <|> numericP

nullP :: Monad m => ObjectParser u m
nullP = string "null" >> return Null

booleanP :: Monad m => ObjectParser u m
booleanP =
  (string "true" >> return (Boolean True))
    <|> (string "false" >> return (Boolean False))

-- * Parses a bytestring and returns either a NumericInt or NumericReal.
numericP :: Monad m => ObjectParser u m
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


