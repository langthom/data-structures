{- |
Module      :   Queue.hs
Description :   Implementation of a Queue.
Copyright   :   (c) Thomas Lang, 2014
License     :   BSD3

Stability   :   stable
Portability :   portable (uses Stack.hs)

This module implements a typical Queue.
This data structure is here a composition of
two Stacks combined.
A Queue allows enqueue-ing (adding) new elements
at the LEFT stack only, while dequeue-ing
(removing) elements on the RIGHT stack only.
 -}
module Queue(
                QUEUE,         -- ^ data type
                emptyQueue,    -- ^ constant 'EmptyQueue'
                queueFromList, -- ^ creating Queue from list
                isQueueEmpty,  -- ^ is Queue empty?
                enqueue,       -- ^ adding new element to Queue
                dequeue        -- ^ removing element from Queue
            )where


import Stack 


-- --------------------------------------------------------------
-- abstract data type representing Queue
-- --------------------------------------------------------------
data QUEUE a = EmptyQueue
             | Q (STACK a) (STACK a)


-- --------------------------------------------------------------
-- nice String representation of a Queue
-- --------------------------------------------------------------
instance (Show a) => Show (QUEUE a) where
    show EmptyQueue = "EmptyQueue"
    show (Q x y)    = show x ++ " | " ++ show y


-- --------------------------------------------------------------
-- instance for checking equality
-- --------------------------------------------------------------
instance (Eq a) => Eq (QUEUE a) where
    EmptyQueue == EmptyQueue = True
    Q x y      == Q u v      = (x == u) && (y == v)
    _          == _          = False



-- sample queue, for testing purposes only
test :: QUEUE Integer
test = queueFromList [1,2,3,4,5,6,7,8,9,10]



-- --------------------------------------------------------------
-- constant function representing 'EmptyQueue'
-- --------------------------------------------------------------
emptyQueue :: QUEUE a
emptyQueue = EmptyQueue


-- --------------------------------------------------------------
-- creates a new Queue from a passed list
-- --------------------------------------------------------------
queueFromList :: [a] -> QUEUE a
queueFromList l = let (x,y) = splitAt ((length l) `div` 2) l
                  in Q (fromList x) (fromList y)



-- --------------------------------------------------------------
-- checks whether a passed queue is empty or not
-- --------------------------------------------------------------
isQueueEmpty :: QUEUE a -> Bool
isQueueEmpty EmptyQueue = True
isQueueEmpty _          = False


-- --------------------------------------------------------------
-- creates a new Queue already containing the new element,
-- adding the element at the left side
-- --------------------------------------------------------------
enqueue :: a -> QUEUE a -> QUEUE a
enqueue b EmptyQueue = Q (push b emptyStack) emptyStack
enqueue b (Q x y)    = Q (push b x) y



-- --------------------------------------------------------------
-- returns a new Queue with removed element on the right side
-- --------------------------------------------------------------
dequeue :: QUEUE a -> QUEUE a
dequeue EmptyQueue = error "Cannot dequeue from empty queue."
dequeue (Q x y)    = case (isEmpty y) of
                        True -> dequeue (Q y (reverseStack x))
                        _    -> case (pop (reverseStack y)) of
                                  Just z  -> Q x z
                                  Nothing -> error "Dequeue error."



-- --------------------------------------------------------------
-- helper function for reversing a stack
-- --------------------------------------------------------------
reverseStack :: STACK a -> STACK a
reverseStack s = let x = reverse $ toList s
                 in fromList x

