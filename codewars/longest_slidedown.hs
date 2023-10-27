module PyramidSlideDown where

longestSlideDown :: [[Int]] -> Int
longestSlideDown (x:[])   = maximum x
longestSlideDown (x:y:ys) = longestSlideDown $ zipWith3 (\a b c -> max a b + c) (0:x) (x++[0]) y : ys 