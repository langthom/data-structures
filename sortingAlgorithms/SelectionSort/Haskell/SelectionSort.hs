module SelectionSort( sort ) where

-- -------------------------------------------------------------
-- Module       :   SelectionSort.hs
-- Description  :   Implementation of Selection sort
-- Copyright    :   (c) Thomas Lang, 2014
-- License      :   BSD3
--
-- Stability    :   stable
-- Portability  :   portable
--
-- This module implements the Selection sort algorith. This one 
-- sorts a list by searching for it's minimum and adds it to a
-- new list while deleting it from the original list.
-- Then this is repeated until the list that has to
-- be sorted is empty.
--
-- runs in  O(n^2)
-- -------------------------------------------------------------

import Data.List  ( minimum, delete )

sort :: (Eq a, Ord a) => [a] -> [a]
sort [] = []
sort l  = m : sort (delete m l)
    where m = minimum l
