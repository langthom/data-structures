module Main( main ) where

{- |
Module      :   Main.hs
Descriptoin :   Testing module for the Quicksort
Copyright   :   (c) Thomas Lang, 2014
License     :   BSD3

Stability   :   stable
Portability :   portable

This module sorts a testing list using the QuickSort algorithm.
 -}

import QuickSort  ( sort )

sample :: [Integer]
sample = [ 5, 9, 24, 72, 0, (-5), 18, 999, 212 ]

main :: IO()
main = do putStrLn "*** List-to-sort: "
          putStrLn $ show sample
          putStrLn "*** Sorting list ...\n  ### Sorted list: "
          putStrLn $ show $ sort sample
