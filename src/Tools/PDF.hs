module Tools.PDF where

import           Data.Char                      ( intToDigit
                                                , toUpper
                                                )

-- | Shows a byte as two hexadecimal digits.
--
--   Beware: Undefined behavior for integers outside the range [0, 255].
showByte16 :: Integral a => a -> String
showByte16 n = map (toUpper . intToDigit . fromIntegral) [div n 16, mod n 16]

-- | Converts a string into a PDF-style hexadecimal string.
--
--   Beware: String containing non ASCII characters might not work.
strToHexadecStr :: String -> String
strToHexadecStr s = '<' : concatMap (showByte16 . fromEnum) s ++ ">"
