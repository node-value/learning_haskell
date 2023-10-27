toPart :: [Char] -> [Char]
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: [Char] -> [Char]
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart :: [Char] -> [Char]
fromPart author = "Thanks,\n" ++ author

createEmail :: [Char] -> [Char] -> [Char] -> [Char]
createEmail recipient bookTitle author = toPart recipient ++ bodyPart bookTitle ++ fromPart author

substract2 :: Integer -> Integer
substract2 = flip (-) 2

subseq :: Int -> Int -> [a] -> [a]
subseq from to list = take (to-from) $ drop from list

myGcd :: Int -> Int -> Int
myGcd a b = if rem == 0 then b else myGcd b rem where rem = a `mod` b

myTake :: Int -> [a] -> [a]
myTake n list = if null list || n == 0 then [] else head list : myTake (n - 1) (tail list)

myDrop :: Int -> [a] -> [a]
myDrop n list = if null list || n == 0 then list else myDrop (n - 1) (tail list)

myLength :: [a] -> Int
myLength list = case list of
    [] -> 0 
    (_:xs) -> myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

ackermann :: (Num t1, Num t2, Eq t1, Eq t2) => t1 -> t2 -> t2
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf x list = x `elem` take (length list `div` 2) list  

main :: IO ()
main = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Title?"
    title <- getLine
    print "Who is the Author?"
    author <- getLine
    print (createEmail recipient title author)