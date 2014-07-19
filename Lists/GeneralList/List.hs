module List( List,              -- ^ abstract data type
             emptyList,         -- ^ constant empty List
             cons,              -- ^ appending new element
             delete,            -- ^ deleting an element
             length,            -- ^ length of list
             elementAt,         -- ^ get element at index
             reverse,           -- ^ reverse list
             concatenate        -- ^ concatenate lists
           ) where

import Prelude hiding ( length, reverse )


-- ----------------------------------------------------------------------------
-- abstract data type with constructors 
--     "NIL" (empty list), and
--     "Cons" (non-empty list)
-- ----------------------------------------------------------------------------
data List a = NIL
            | Cons a (List a) 
            


-- -----------------------------------------------------------------------------
-- creating output just like Haskell-lists
-- -----------------------------------------------------------------------------
instance Show a => Show (List a) where
    show NIL = "[]"
    show l   = "[" ++ showList l ++ "]"
        where showList NIL          = ""
              showList (Cons x NIL) = show x 
              showList (Cons x y)   = show x ++ ", " ++ showList y


-- -----------------------------------------------------------------------------
-- comparing for equality
-- -----------------------------------------------------------------------------
instance Eq a => Eq (List a) where
    NIL        == NIL        = True
    (Cons x y) == (Cons u v) = (x == u) && (y == v)
    _          == _          = False


-- ------------------------------------------------------------------------------
-- constant empty-list-function
-- ------------------------------------------------------------------------------
emptyList :: List a
emptyList = NIL


-- ------------------------------------------------------------------------------
-- appends a new element to the front of a list
-- ------------------------------------------------------------------------------
cons :: a -> List a -> List a
cons x NIL = Cons x NIL
cons x l   = Cons x l


-- ------------------------------------------------------------------------------
-- deletes an element from a list
-- ------------------------------------------------------------------------------
delete :: Eq a => a -> List a -> List a
delete x NIL                    = error "Error! Cannot delete from empty List."
delete x (Cons u v) | x == u    = v
                    | otherwise = Cons u (delete x v)


-- ------------------------------------------------------------------------------
-- returns the length of a list
-- ------------------------------------------------------------------------------
length :: List a -> Int
length NIL        = 0
length (Cons x y) = 1 + length y


-- ------------------------------------------------------------------------------
-- returns the element of a list at a passed position
-- ------------------------------------------------------------------------------
elementAt :: Eq a => Int -> List a -> Maybe a
elementAt _ NIL                    = Nothing
elementAt 0 (Cons x y)             = Just x
elementAt i (Cons x y) | i >= 0    = elementAt (i-1) y
                       | otherwise = Nothing


-- ------------------------------------------------------------------------------
-- reverses a list
-- ------------------------------------------------------------------------------
reverse :: List a -> List a
reverse NIL = NIL
reverse (Cons u v) = concatenate (reverse v) (Cons u NIL)


-- ------------------------------------------------------------------------------
-- concatenates two lists
-- ------------------------------------------------------------------------------
concatenate :: List a -> List a -> List a
concatenate NIL NIL = NIL
concatenate x   NIL = x
concatenate NIL y   = y
concatenate (Cons a b) l = Cons a (concatenate b l)


