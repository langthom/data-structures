{- |
Module      :   Stack.hs
Description :   Implementation of a Stack
Copyright   :   (c) Thomas Lang, 2014
License     :   BSD3

Stability   :   stable
Portability :   portable

This module implements a classical Stack with an abstract
data type. It supports all typical functions, like "push",
"pop" and "isEmpty".
Furthermore, it gives the opportunity to create a stack from
a list.
 -}

module Stack (  emptyStack,     -- ^ constant function for Empty Stack
                isEmpty,        -- ^ checks if stack is empty or not
                fromList,       -- ^ creates a Stack from list
                push,           -- ^ Pushes an element to a Stack
                pop,            -- ^ Pops the Stack
                top             -- ^ Returns the top element of a Stack
             ) where



-- --------------------------------------------------------
-- Abstract data type
-- --------------------------------------------------------
data STACK a = EmptyStack
             | S a (STACK a)
             deriving (Ord)


-- --------------------------------------------------------
-- instance for checking the equality of two Stacks
-- --------------------------------------------------------
instance (Eq a) => Eq (STACK a) where
    x == y  =  x `equalsTree` y


-- -------------------------------------------------------
-- instance for pretty-printing a Stack
-- -------------------------------------------------------
instance (Show a) => Show (STACK a) where
    show EmptyStack = "EmptyStack"
    show (S x y)    = "Stack " ++ show x ++ " ( " ++ show y ++ ")"


-- --------------------------------------------------------
-- Constant function, creates empty Stack
-- --------------------------------------------------------
emptyStack :: STACK a
emptyStack = EmptyStack
 

-- --------------------------------------------------------
-- Checks whether a Stack is empty or not
-- --------------------------------------------------------
isEmpty :: STACK a -> Bool
isEmpty EmptyStack = True
isEmpty _          = False


-- --------------------------------------------------------
-- private functions that checks the equality
-- --------------------------------------------------------
equalsTree :: (Eq a) => STACK a -> STACK a -> Bool
equalsTree EmptyStack EmptyStack       = True
equalsTree (S x y)    (S u v) | x == u = equalsTree y v
equalsTree _           _               = False


-- --------------------------------------------------------
-- Creates a Stack from a list
-- --------------------------------------------------------
fromList :: [a] -> STACK a
fromList []     = EmptyStack
fromList (x:xs) = S x (fromList xs )


-- --------------------------------------------------------
-- Pushes a new element to a stack
-- --------------------------------------------------------
push :: a -> STACK a -> STACK a
push x EmptyStack = S x EmptyStack
push x s          = S x s


-- --------------------------------------------------------
-- Pops the stack, so it removes the top element
-- --------------------------------------------------------
pop :: STACK a -> Maybe (STACK a)
pop EmptyStack = Nothing
pop (S x y)    = Just y


-- -------------------------------------------------------
-- Returns the top element of the stack
-- -------------------------------------------------------
top :: STACK a -> Maybe a
top EmptyStack = Nothing
top (S x y)    = Just x
