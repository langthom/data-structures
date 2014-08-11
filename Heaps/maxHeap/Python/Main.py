#!/usr/bin/env python

from MaxHeap import *

def main():
    """Main method, does some testing."""
    print( "*** Initializing new MaxHeap ... done." )
    print( "*** Adding values 7, 42, 999, 0, 212, 512, 21 ... done." )

    test = MaxHeap()

    test.insert( 7   )
    test.insert( 42  )
    test.insert( 999 )
    test.insert( 0   )
    test.insert( 212 )
    test.insert( 512 )
    test.insert( 21  )

    print( "   ### Current MaxHeap: " )
    test.println()
    print( "\n   ### Number of Nodes: " + str( test.getSize() ))

    print( "\n*** Performing \"deleteMax\" ... done." )
    test.deleteMax()

    print( "   ### Current MaxHeap: " )
    test.println()
    print( "\n   ### Number of Nodes: " + str( test.getSize() ))


if __name__ == '__main__':
    main()

