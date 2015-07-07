{- |
 - Module      :  Set.hs
 - Description :  Implementation of data structure 'Set'.
 - Copyright   :  (c) Thomas Lang
 - License     :  BSD3
 -
 - Stability   :  stable
 - Portability :  portable
 -
 - Implementation of the typical data structure 'Set'.
 -}
 module Set (
    Set(..),
    add,
    delete,
    clear,
    size,
    isempty,
    contains,
    union,
    intersect,
    difference,
    subsetOf
) where

import Data.List ((\\), group, sort, isInfixOf)

-- | Data type representing a set.
data Set a = S [a] | EmptySet  deriving (Ord, Eq)

-- | Instance for pretty-printing a set.
instance Show a => Show (Set a) where
    show (S l) = show l
    show EmptySet = "[]"

-- | Adds the passed element to the passed set.
add :: Eq a => Set a -> a -> Set a
add EmptySet x = S [x]
add s@(S l)    x | not (x `elem` l) = S (x:l)
                 | otherwise        = s

-- | Deletes the passed element from the passed set if it was contained in it, 
-- otherwise is does nothing.
delete :: Eq a => Set a -> a -> Maybe (Set a)
delete EmptySet _ = Nothing
delete s@(S l) x | x `elem` l = Just $ S (l \\ [x])
                 | otherwise  = Nothing

-- | Clears the set.
clear :: Set a -> Set a
clear EmptySet = EmptySet
clear (S _) = S []

-- | Computes the number of elements contained in the set.
size :: Set a -> Int
size EmptySet = 0
size (S l) = length l

-- | Checks if the passed set is empty.
isempty :: Set a -> Bool
isempty EmptySet = True
isEmpty (S [])   = True
isEmpty _        = False

-- | Checks if the passed set contains the passed element.
contains :: Eq a => Set a -> a -> Bool
contains EmptySet _ = False
contains (S l) x    = x `elem` l

-- | Computes the union of the two passed sets.
-- This is a set containing all elements from both the first and the second passed sets
-- but without any duplicates.
union :: Ord a => Set a -> Set a -> Set a
union EmptySet t = t
union s EmptySet = s
union s t = S $ rmdups $ s ++ t
    where rmdups = map head . group . sort

-- | Computes the intersection of the two passed sets.
-- This is the subset that contains elements, that appear both in the first and the second
-- passed set.
intersect :: Ord a => Set a -> Set a -> Set a
intersect EmptySet _ = EmptySet
intersect _ EmptySet = EmptySet
intersect (S s) (S t) = S $ map head . filter (\l -> tail l /= []) . group $ sort (s ++ t)

-- | Computes the difference of the two passed sets.
-- This are all elements from the first set that do not appear in the second set.
difference :: Eq a => Set a -> Set a -> Set a
difference EmptySet _ = EmptySet
difference x EmptySet = x
difference (S x) (S y) = S (x \\ y)

-- | Decides, if the fist set is a subset of the second one.
subsetOf :: Eq a => Set a -> Set a -> Bool
subsetOf EmptySet _ = True
subsetOf _ EmptySet = False
subsetOf (S x) (S y) = x `isInfixOf` y

