greatings :: String
greatings = "Hello, this programm will give you an n'th fibonacci number"

request :: String
request = "Please, enter an n number: "

respond :: Int -> Maybe Integer -> String
respond n (Just result) = helper n (show result)
respond n Nothing       = helper n "undefind"

helper :: Int -> String -> String
helper n str = mconcat ["The ", show n, " fibonacci number is ", str]

nthFib :: Int -> Maybe Integer
nthFib n = if n > 0 then Just (head $ reverse $ take n fibStream) else Nothing 

fibStream :: [Integer]
fibStream = fibHelp 0 1 where 
    fibHelp a b = a : fibHelp b (a+b)

main :: IO ()
main = do 
    putStrLn greatings
    putStrLn request
    nStr <- getLine
    let n = read nStr
    putStrLn $ respond n (nthFib n)