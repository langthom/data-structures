module UsesBinTree where

import BinTree

sample :: BinTree Integer
sample = Node ( Node emptyTree 4 emptyTree ) 7 ( Node emptyTree 17 emptyTree )

bigSample :: BinTree Integer
bigSample = Node ( Node ( Node emptyTree 0 emptyTree ) 4 ( Node emptyTree 5 emptyTree ) ) 7 ( Node ( Node emptyTree 10 emptyTree ) 24 ( Node emptyTree 42 emptyTree ) )
