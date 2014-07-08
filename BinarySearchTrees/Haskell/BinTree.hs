module BinTree( BinTree(..),        -- ^ data type
                emptyTree,          -- ^ empty tree constant
                isEmpty,            -- ^ is tree empty or not?
                insert,             -- ^ inserting in tree
                delete,             -- ^ deleting from tree
                preorder,           -- ^ traversing preorder
                inorder,            -- ^ traversing inorder
                postorder,          -- ^ traversing postorder
                atLevel             -- ^ return a list of all 
                                    --   elements on the same tree-level
              )
where


-- Binary search tree 
-- params: a -> type of saved information
data BinTree a = EmptyTree
               | Node (BinTree a) a (BinTree a)
               deriving (Ord, Eq)


instance (Show a) => Show (BinTree a) where
    show EmptyTree    = " Empty tree"
    show (Node l o r) = "( Node: " ++ show o ++ ")\n( Left:  " ++ show l ++ " , Right:  " ++ show r ++ " )"


-- constant function representing the empty tree
-- (will be the leaves)
emptyTree :: BinTree a
emptyTree = EmptyTree


-- checks wheter a tree is empty or not
isEmpty :: BinTree a -> Bool
isEmpty EmptyTree = True
isEmpty _         = False


-- inserting a new value into the BinTree
-- note: as we use the >= relation for the
--       right subtree, there can be 
--       duplicates in this tree
insert :: (Num a, Ord a) => a -> BinTree a -> BinTree a
insert value EmptyTree                 = Node EmptyTree value EmptyTree
insert value (Node l o r) | value < o  = Node (insert value l) o r
                          | value >= o = Node l o (insert value r)


-- deleting a value from a tree
delete :: (Ord a) => BinTree a -> a -> BinTree a
delete EmptyTree _                                       = error "Nothing to delete."
delete (Node EmptyTree o EmptyTree) value | value == o   = emptyTree
                                          | otherwise    = error "Element not found"
delete (Node l o r) value | value < o                    = Node (delete l value) o r
                          | value > o                    = Node l o (delete r value)



----------------------------- Traversing tree --------------------------------

-- traversing the tree in preorder notation
preorder :: BinTree a -> [a]
preorder EmptyTree    = []
preorder (Node l o r) = [o] ++ preorder l ++ preorder r


-- traversing the tree in inorder notation
-- note: this returns a sorted list, because
--       of the property of a binary search tree
inorder :: BinTree a -> [a]
inorder EmptyTree    = []
inorder (Node l o r) = inorder l ++ [o] ++ inorder r


-- traversing the tree in postorder notation
postorder :: BinTree a -> [a]
postorder EmptyTree    = []
postorder (Node l o r) = postorder l ++ postorder r ++ [o]


-------------------------------------------------------------------------------

-- returns a list of all elements on the same tree-level
atLevel :: BinTree a -> Int -> [a]
atLevel EmptyTree _    = []
atLevel (Node l x r) 0 = [x]
atLevel (Node l x r) a = atLevel l (a-1) ++ atLevel r (a-1)
