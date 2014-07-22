module Main( main ) where

import qualified List as L

sample :: L.List Int
sample = L.fromList [ 0, 42, 24, 7, 999 ]

main :: IO()
main = do putStrLn "*** New list from Haskell-list ... done."
          putStrLn $ "   ### Current list: " ++ show sample
          putStrLn $ "*** Is this list empty? ->" ++ show (L.null sample)
          putStrLn "It is up to the interested one to do further testing ;-)"

