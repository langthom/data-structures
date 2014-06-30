module InsertionSort( sort ) where

-- -----------------------------------------------------------
-- Module       :   InsertionSort.hs
-- Description  :   Implementation of the Insertion sort
-- Copyright    :   (c) Thomas Lang, 2014
-- License      :   BSD3
--
-- Stability    :   stable
-- Portability  :   portable
--
-- This module implements a function that performs the
-- "Insertion sort" algorithm on a list.
-- The insertion sort algorithm compares elements an then
-- it moves that element to it's correct position.
--
-- -> Runs in O(n^2)
-- -----------------------------------------------------------

import Data.List  ( insert )

sort :: (Ord a) => [a] -> [a]
sort = foldr insert []
