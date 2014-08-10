module Main( main ) where

import Queue

test :: QUEUE Integer
test = queueFromList [1,2,3,4,5]

main :: IO()
main = do putStrLn "*** Creating new Queue ... done."
          putStrLn "   ### Current Queue:"
          putStr "        "
          putStrLn $ show test
          putStrLn "*** Performing dequeue ... done."
          putStrLn "   ### Current Queue:"
          putStr "        "
          putStrLn $ show $ dequeue test
          putStrLn "*** Enqueue-ing value 999 ... done."
          putStr "        "
          putStrLn $ show $ enqueue 999 $ dequeue test

