{- |
Module      :   Graph.hs
Description :   Implementation of a simple Graph.
Copyright   :   (c) Thomas Lang, 2014
License     :   BSD 3

Stability   :   stable
Portability :   portable

This module implements a typical DIRECTED graph.
Such a graph is simple a list of edges, while an 
edge is a tuple from the first Node to the second
one. Furthermore it implements a few functions
and algorithms that are typical for graphs, like
the testing if two nodes are adjacent, or the
depth-first search.
 -}
module Graph(
              Graph,        -- ^ data type
              adj,          -- ^ are two nodes adjacent?
              adjList,      -- ^ adjList of a Node
              contains,     -- ^ is a node in the graph?
              add,          -- ^ adds a new edge to graph
              remove        -- ^ removes a node from the graph
            )where

import Data.List 


-- --------------------------------------------------------------
-- type synonyms representing a directed graph
-- --------------------------------------------------------------
type Node a = a

type Edge a = (Node a,Node a)

type Graph a = [Edge a]


-- sample graph (for testing purposes only)
test :: Graph Integer
test = [(1,2),(2,3),(2,2),(1,4),(4,4)]



-- --------------------------------------------------------------
-- checks if two passed nodes are adjacent
-- --------------------------------------------------------------
adj :: Eq a => Node a -> Node a -> Graph a -> Bool
adj v w g = (v,w) `elem` g



-- --------------------------------------------------------------
-- returns a list of nodes that are adjacent to the passed node
-- --------------------------------------------------------------
adjList :: Eq a => Node a -> Graph a -> [Node a] 
adjList v g = let x = filter (\(x,y) -> x==v) g
              in map snd x


-- --------------------------------------------------------------
-- checks if the passed graph contains the passed node
-- --------------------------------------------------------------
contains :: Eq a => Graph a -> Node a -> Bool
contains []         _             = False
contains ((x,_):xs) n | n == x    = True
                      | otherwise = contains xs n


-- --------------------------------------------------------------
-- adds a new edge to the graph
-- --------------------------------------------------------------
add :: Edge a -> Graph a -> Graph a
add e g = e : g


-- --------------------------------------------------------------
-- removes a node from the graph
-- --------------------------------------------------------------
remove :: Eq a => Node a -> Graph a -> Maybe (Graph a)
remove n [] = Nothing
remove n g | g `contains` n = Just $ g \\ filter (\(u,_) -> u == n) g
           | otherwise      = Nothing
