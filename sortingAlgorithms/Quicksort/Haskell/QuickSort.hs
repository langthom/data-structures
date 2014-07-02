module QuickSort( quicksort ) where

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [z | z <- xs, z > x ]
