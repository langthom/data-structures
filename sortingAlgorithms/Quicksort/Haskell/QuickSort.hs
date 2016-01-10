{-|
Module      :   QuickSort.hs
Description :   Implementation of a parallel quicksort.
Copyright   :   (c) Thomas Lang, 2016
License     :   BSD3

Stability   :   stable
Portability :   Uses Control.Parallel, Data.List

Implementation of a simple quicksort algorithm.
 -}
module QuickSort ( sort, force ) where

import Control.Parallel
import Data.List         (partition)

-- force deep evaluation of entire list
force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:cs) = go cs
          go []     = 1

seqSort :: Ord a => [a] -> [a]
seqSort []     = []
seqSort (x:xs) = let (lef, gre) = partition (<=x) xs
                 in seqSort lef ++ x : seqSort gre


parSort :: Ord a => Int -> [a] -> [a]
parSort level list@(x:xs)
    | level <= 0 = seqSort list
    | otherwise  = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where lesser  = parSort level' [ y | y <- xs, y <= x ]
          greater = parSort level' [ z | z <- xs, z > x  ]
          level'  = level - 1
parSort _ _ = []

sort :: Ord a => [a] -> [a]
sort = parSort 2

