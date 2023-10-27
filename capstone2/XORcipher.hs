module XORcipher where

type Bits = [Bool]

newtype OneTimePad = OTP String

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && not (value1 && value2)

xor :: Bits -> Bits -> Bits
xor = zipWith xorBool

intToBits' :: Int -> Bits
intToBits' n = if n < 2 then [toEnum n] else toEnum (n `mod` 2) : intToBits' (n `div` 2)

maxBits :: Int
maxBits = length $ intToBits' maxBound

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits  = reverse $ intToBits' n
          leadingFalses = replicate (maxBits - length reversedBits) False

bitsToInt :: Bits -> Int
bitsToInt bits = sum $ map (\x -> 2 ^ snd x) trueLoc
    where trueLoc = filter fst (zip bits ind)
          ind     = [size - 1, size - 2 .. 0]
          size    = length bits

charToBits :: Char -> Bits
charToBits char = intToBits $ fromEnum char

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

applyOTP :: String -> String -> String
applyOTP pad plainT = map bitsToChar $ zipWith xor (map charToBits pad) (map charToBits plainT)
