module Main where

import BinTree

sample :: BinTree Integer
sample = Node ( Node emptyTree 4 emptyTree ) 7 ( Node emptyTree 17 emptyTree )

bigSample :: BinTree Integer
bigSample = Node ( Node ( Node emptyTree 0 emptyTree ) 4 ( Node emptyTree 5 emptyTree ) ) 7 ( Node ( Node emptyTree 10 emptyTree ) 24 ( Node emptyTree 42 emptyTree ) )


main :: IO()
main = do putStrLn "*** Creating new Tree ..."
          putStrLn "*** Is tree empty? -> " 
          putStrLn $ show $ isEmpty sample
          putStrLn "  ### Actual tree: \n"
          putStrLn $ show sample
          putStrLn "*** Inserting value \"1\" ...\n" 
          putStrLn "  ### Actual tree: \n"
          putStrLn $ show $ insert 1 sample
          putStrLn "\n Deleting element 17 ...\n"
          putStrLn $ show $ delete (insert 1 sample) 17
          putStrLn "----------------------------------"
          putStrLn "        Traversing"
          putStr   "\n Preorder: " 
          putStrLn $ show (preorder sample)
          putStr   "Inorder: "
          putStrLn $ show (inorder sample)
          putStr   "Postorder: "
          putStrLn $ show (postorder sample)

