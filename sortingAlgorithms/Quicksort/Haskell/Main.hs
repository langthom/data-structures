{- |
Module      :   Main.hs
Descriptoin :   Testing module for the Quicksort
Copyright   :   (c) Thomas Lang, 2016
License     :   BSD3

Stability   :   stable
Portability :   Uses System.Random, Data.Time.Clock

This module sorts a testing list using the QuickSort algorithm.
 -}

module Main (main) where

import QuickSort

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Random   (StdGen, getStdGen, randoms)

randomInts :: Int -> StdGen -> [Int]
randomInts k g = let result = take k (randoms g)
                 in force result `seq` result

main :: IO()
main = do
    input <- randomInts 1000000 `fmap` getStdGen
    putStrLn "There are 1000000 random integers to sort."
    start <- getCurrentTime
    let sorted = sort input
    putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."
    end <- getCurrentTime
    putStrLn $ "Needed " ++ show (end `diffUTCTime` start) ++ " seconds for that."

