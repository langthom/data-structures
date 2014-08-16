{- |
Module      :   Graph.hs
Description :   Implementation of a simple Graph.
Copyright   :   (c) Thomas Lang, 2014
License     :   BSD 3

Stability   :   stable
Portability :   portable

This module implements a typical DIRECTED AZYCLICAL 
graph.
Such a graph is simple a list of edges, while an
edge is a tuple from the first Node to the second
one. Furthermore it implements a few functions
and algorithms that are typical for graphs, like
the testing if two nodes are adjacent, or the
topologic sorting algorithm.
 -}
module Graph(
              Graph,        -- ^ data type
              adj,          -- ^ are two nodes adjacent?
              adjList,      -- ^ adjList of a Node
              contains,     -- ^ is a node in the graph?
              add,          -- ^ adds a new edge to graph
              remove,       -- ^ removes a node from the graph
              topSort,      -- ^ topological sorting
              isAzyclical   -- ^ is graph azyclical?
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
test = [(1,2),(2,3),(1,4),(2,4)]



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
-- returns the size of the graph ( the number of nodes in it )
-- --------------------------------------------------------------
size :: Graph a -> Int
size = length


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




-- ---------------------------------------------------------------
-- performs the topological sorting algorithm
-- ---------------------------------------------------------------
topSort :: Eq a => Graph a -> [Node a]
topSort g = let x = ts g
                y = map fst x ++ map snd x
            in nub y
    where ts :: Eq a => Graph a -> Graph a 
          ts [] = []
          ts g  = let f = sources g 
                  in f ++ ts (g \\ f)



-- ---------------------------------------------------------------
-- gets all edges which first nodes are
-- sources (which has no ingoing edges)
-- ---------------------------------------------------------------
sources :: Eq a => Graph a -> Graph a
sources g = filter (\e -> not (fst e `elem` f)) g
    where f = map snd g



-- ---------------------------------------------------------------
-- checks if the passed graph is azyclical.
-- a graph is azyclical, if the graph reduced
-- by all edges given through topSort is 
-- the empty list.
-- ---------------------------------------------------------------
isAzyclical :: Eq a => Graph a -> Bool
isAzyclical g = g \\ ts == []
    where ts = [(u,v)|u <- topSort g, v <- s]
          s  = [x | let a = map snd g, x <- a]


