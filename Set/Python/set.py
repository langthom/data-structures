#!/usr/bin/python3

from BinarySearchTree import *

class Set:
    """Implementation of the set data structure.
    A Set here uses a 'BinarySearchTree' as the 
    internal data container.
    A Set is basically a data container that allows 
    no duplicates in it.

    @author Thomas Lang
    @version 1.0, 06/02/2015
    """

    def __init__(self):
        """Creates a new Set by instantiating the BinarySearchTree."""
        self.tree = BinarySearchTree()

    def isEmpty(self):
        """Checks if the Set has no elements in it.
        
        @return True if the set is empty, False otherwise. """
        return self.tree.isEmpty()

    def size(self):
        """Returns the total number of elements contained in the set.
        
        @return The size of the set. """
        return self.tree.getSize()

    def union(self, other):
        """Returns the union of this set and the passed other.
        The union of two sets describes all elements that are either
        stored in the first or the second one, or both.
        
        @return The union of the two sets. """

        if other is None:
            raise ValueError("Second set must not be null")

        unionedSet = Set()
        l = self.__getAll()
        l += other.__getAll()
        l.sort()
        unionedSet.addAll(l)
        return unionedSet

    def intersect(self, other):
        """Returns the intersection of this set and the passed other.
        The intersection of two sets describes all elements that are
        both stored in the first and the second one.
        
        @return The intersection of the two sets."""

        if other is None:
            raise ValueError("Second set must not be null")

        intersectedSet = Set()
        l = other.__getAll()

        for item in self.__getAll():
            if item in l:
                intersectedSet.add(item)

        return intersectedSet

    def difference(self, other):
        """Returns this set reduced by the passed other set.
        The difference between two sets A and B describes all
        elements, that are stored in A but not in B.
        
        @return The difference of this set by the passed other."""

        if other is None:
            raise ValueError("Second set must not be null")

        differenceSet = Set()
        l = other.__getAll()

        for item in self.__getAll():
            if not item in l:
                differenceSet.add(item)

        return differenceSet

    def __getAll(self):
        """Returns a list containing all elements in the set."""
        return self.tree.getAll()

    def add(self, value):
        """Adds the passed value to the set"""
        return self.tree.add(value)

    def addAll(self, collection):
        """Adds all elements from the passed collection to this set."""
        if collection is None:
            raise ValueError("Passed collection must not be null")

        for item in collection:
            self.add(item)

    def contains(self, element):
        """Checks if the passed element is contained in this set.
        
        @return True if the elements is in this set, False otherwise."""
        return self.tree.contains(element)

    def remove(self, element):
        """Deletes the passed element from this set."""
        return self.tree.delete(element)

    def isSubsetOf(self, other):
        """Checks if this set is a subset of the passed one.
        
        @return True if it is, False otherwise."""

        if other is None:
            raise ValueError("Second set must not be null")

        l = other.__getAll()
        isSubset = True

        for item in self.__getAll():
            if not item in l:
                isSubset = False
                break

        return isSubset

    def hasSubset(self, other):
        """Checks if the passed set is a subset of this one.
        
        @return True if it is, False otherwise."""

        if other is None:
            raise ValueError("Second set must not be null")

        lall = self.__getAll()
        hasSubset = True

        for item in other.__getAll():
            if not item in lall:
                hasSubset = False
                break

        return hasSubset

    def clear(self):
        """Removes all elements from this set."""
        self.tree = BinarySearchTree()

    def __str__(self):
        """Returns a String representation of this set."""
        if self.isEmpty():
            return "(empty set)"
        return self.tree.__str__()

def main():
    """Testing function."""
    A = Set()
    B = Set()

    for i in range(1,5):
        A.add(i*i)

    for i in range(1,21):
        B.add(i)

    print("*** Creating two new sets ... done.")
    print("*** Initializing with values ... done.")
    print("Set A: ", end="")
    print(A)
    print("Size of A: ")
    print(A.size())
    print()
    print("Set B: ", end="")
    print(B)
    print("Size of B: ", end="")
    print(B.size())
    print()
    print("A intersects B: ", end="")
    print(A.intersect(B))
    print("A unioned with B: ", end="")
    print(A.union(B))
    print()
    print("A \\ B: ", end="")
    print(A.difference(B))
    print("B \\ A: ", end="")
    print(B.difference(A))
    print()
    print("A subset of B? -> ", end="")
    print(A.isSubsetOf(B))
    print("B subset of A? -> ", end="")
    print(B.isSubsetOf(A))
    print()
        
    oneFourSet = Set()
    oneThreeSet = Set()
    oneFourSet.add(1)
    oneFourSet.add(4)
    oneThreeSet.add(1)
    oneThreeSet.add(3)

    print("A has subset {1,4}? ", end="")
    print(A.hasSubset(oneFourSet))
    print("A has subset {1,3}? ", end="")
    print(A.hasSubset(oneThreeSet))
    print()
    print("*** Clearing set A ... done.")
      
    A.clear()

    print("Set A now: ", end="")
    print(A)


if __name__ == '__main__':
    main()
