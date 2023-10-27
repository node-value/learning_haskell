module Cipher where

import ROTcipher (rotEncoder, Rot(Rot))
import XORcipher (applyOTP, OneTimePad(OTP), myOTP)

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

instance Cipher Rot where 
    encode :: Rot -> String -> String
    encode Rot = rotEncoder 
    decode :: Rot -> String -> String
    decode Rot = rotEncoder

instance Cipher OneTimePad where
  encode :: OneTimePad -> String -> String
  encode (OTP pad) = applyOTP pad 
  decode :: OneTimePad -> String -> String
  decode (OTP pad) = applyOTP pad