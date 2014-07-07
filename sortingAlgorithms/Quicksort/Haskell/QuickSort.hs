module QuickSort( sort ) where

{-|
Module      :   QuickSort.hs
Description :   Implementation of QuickSort
Copyright   :   (c) Thomas Lang, 2014
License     :   BSD3

Stability   :   stable
Portability :   portable

This module implements the Quick-Sort algorithm.
 -}

sort :: (Ord a) => [a] -> [a]
sort []     = []
sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++ sort [z | z <- xs, z > x ]
