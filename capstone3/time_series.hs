import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1  = [ (1, 200.1), (2, 199.5), (3, 199.4), (4, 198.9), (5, 199.0), (6, 200.2), (9, 200.3), (10, 201.2), (12, 202.9)]
file2 :: [(Int,Double)]
file2  = [(11, 201.6), (12, 201.5), (13, 201.5),(14, 203.5), (15, 204.9), (16, 207.1),(18, 210.5), (20, 208.8)]
file3 :: [(Int,Double)]
file3  = [(10, 201.2), (11, 201.6), (12, 201.5),(13, 201.5), (14, 203.5), (17, 210.5),(24, 215.1), (25, 218.7)]
file4 :: [(Int,Double)]
file4  = [(26, 219.8), (27, 220.5), (28, 223.8),(29, 222.8), (30, 223.8), (31, 221.7),(32, 222.3), (33, 220.8), (34, 219.4), (35, 220.1), (36, 220.6)]

data TS a = TS [Int] [Maybe a]

type CompareFunc   a = a -> a -> a
type TSCompareFunc a = (Int, Myabe a) -> (Int, Myabe a) -> (Int, Myabe a)

instance Show a => Show (TS a) where
    show (TS times values) = mconcat $ zipWith showTVPair times values

instance Semigroup (TS a) where
    (<>) = combineTS

instance Monoid (TS a) where
    mempty  = TS [] []
    mappend = (<>)

ts1 :: TS Double
ts1 = fileToTs file1

ts2 :: TS Double
ts2 = fileToTs file2

ts3 :: TS Double
ts3 = fileToTs file3

ts4 :: TS Double
ts4 = fileToTs file4

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
    where completeTimes  = [minimum times .. maximum times]
          timeValueMap   = Map.fromList (zip times values) 
          extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

fileToTs :: [(Int, a)] -> TS a
fileToTs tvPairs = let (times, values) = unzip tvPairs in createTS times values

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time x = case x of 
    Just value -> mconcat [show time, "|", show value, "\n"]
    Nothing    -> show time ++ "|NA\n"

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v 
insertMaybePair map, pair = case pair of 
    (_, Nothing)  -> map
    (k, (Just v)) -> Map.insert k v map

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2        = ts2
combineTS ts1        (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where both times     = mconcat [t1, t2]
          completeTimes  = [minimum bothTimes .. maximum bothTimes]
          tvMap          = foldl insertMaybePair Map.empty (zip t1 v1)
          updatedMap     = foldl insertMaybePair tvMap     (zip t2 v2)
          combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

mean :: (Real a) => [a] -> Double
mean xs = total/count where
    total = (realToFrac . sum)    xs
    count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS t v ) = if all (== Nothing) v then Nothing else Just avg
    where justVals  = filter isJust values
          cleanVals = map formJust justVals
          avg       = mean cleanVals

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = newF
    where newF (i1, Nothing)    (i2, Nothing)   = (i1, Nothing)
          newF (_,  Nothing)    (i , val)       = (i, val)
          newF (i,  val)        (_,  Nothing)   = (i, val)
          newF (i1, Just val1), (i2, Just val2) = if f val1 val2 == val1 then (i1, Just val1) else (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS f (TS [] []) = Nothing
compareTS f (TS t  v)  = if all (== Nothing) values then Nothing else Just best
    where best = foldl (makeTSCompare func) (0, Nothing) (zip times values)

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _         = Nothing
diffPair _ Nothing         = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS t  v)  = TS t (Nothing:diffValues)
    where diffValues = zipWith diffPair (tail v) v