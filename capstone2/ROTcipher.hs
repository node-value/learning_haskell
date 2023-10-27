module ROTcipher where

data Rot = Rot

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN n char = toEnum  ((n `mod` 2 + fromEnum char + n `div` 2) `mod` n)

maxCharN :: Int
maxCharN = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar = rotN (1 + maxCharN)

rotEncoder :: String -> String
rotEncoder = map rotChar
