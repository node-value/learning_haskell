module TitleCase (titleCase) where

import Data.Char
import qualified Data.Set as Set

titleCase :: String -> String -> String
titleCase _ [] = []
titleCase minor title = unwords $ capitalize (head wordedIn) : map decide (tail wordedIn) where
    wordedIn    = words title
    decide word =  if Set.member (lowerize word) (minorSet minor) then lowerize word else capitalize word

minorSet :: String -> Set.Set String
minorSet minor = Set.fromList (map lowerize (words minor))

capitalize :: String -> String
capitalize (x:xs) = toUpper x : map (toLower) xs

lowerize :: String -> String
lowerize = map toLower