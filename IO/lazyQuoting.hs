import qualified Data.Map as Map
import Data.Char

quotesMap :: Map.Map Int String
quotesMap = Map.fromList[(1, "1"), (2, "2"), (3, "3"), (4, "4"), (5, "5")]

quotesCnt :: Int
quotesCnt = 5

request :: String
request = "Please, enter number from 1 to " ++ show quotesCnt ++ " to get a quote, or 'n' to stop programm."

getQuote :: String -> String
getQuote []  = "Please, enter a number!"
getQuote "n" = "Terminating now..."
getQuote x   = if isDigit $ head x then getQuoteHelper $ Map.lookup (read x) quotesMap else "Not a digit" where
    getQuoteHelper x = case x of 
        Just str -> str
        Nothing  -> "Wrong number"

mapInputStream :: [String] -> [String]
mapInputStream []       = []
mapInputStream ("n":xs) = []
mapInputStream (x:xs)   = (getQuote x ++ "\n" ++ request) : mapInputStream xs

main :: IO ()
main = do 
    userInput <- getContents
    putStrLn request
    mapM_ putStrLn $ mapInputStream $ lines userInput
