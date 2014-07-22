#!/usr/env python

def bubbleSort( listToSort ):
    """Sorts the passed list using the Bubble-
    sort algorithm.
    This sorting algorithm swaps two neighboured
    elements in the list, if they are not sorted.
    This is done until all elements are sorted.
    This implementation runs worst-case in O(n^2).
    
    @param  listToSort  The list to sort
    """
    for i in range( len(listToSort), 0, -1 ):
        for j in range( 0, i-1 ):
            if listToSort[j] > listToSort[j+1]:
                tmp = listToSort[j]
                listToSort[j] = listToSort[j+1]
                listToSort[j+1] = tmp


def main():
    """Testing method for the sorting algorithm."""

    import random
    print( "*** Initializing new list ... done. " )
    print( "*** Filling in 20 random values ... done." )

    l = []

    for i in range( 20 ):
        l.append( random.randint( 0, 100 ))

    print( "   ### Unsorted list: " )
    print( l )

    print( "\n*** Sorting the list with Bubble Sort ... done." )
    bubbleSort( l )

    print( "   ### Sorted list: ")
    print( l )

if __name__ == '__main__':
    main()

