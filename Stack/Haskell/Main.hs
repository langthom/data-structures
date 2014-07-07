{- |
Module      :   Main.hs
Description :   Testing module for Stack
Copyright   :   (c) Thomas Lang, 2014
License     :   BSD3

Stability   :   stable
Portability :   portable

This module performs every Stack-action on a pre-formed
stack created from a list.
 -}

module Main( main ) where

import Stack

sample = fromList [ 5, 4, 3, 2, 1, 0 ]

main :: IO()
main = do putStrLn "*** Creating new Stack ..."
          putStrLn "\n  ### Actual Stack: \n"
          putStrLn (show sample)
          putStr   "\nIs the Stack empty? -> " 
          putStr   (show (isEmpty sample))
          putStr   "\n"
          putStrLn "\n*** Push element \"7\" to Stack ..."
          putStrLn $ show $ push 7 sample
          putStr   "\n*** Top element: " 
          putStr   (show (top sample))
          putStr   "\n"
          putStrLn "*** Pop top element ...\n"
          putStrLn $ show $ pop sample
