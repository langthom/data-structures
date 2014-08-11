#!/usr/bin/env python

from MinHeap import *

def main():
    """Main method, does some testing."""
    print( "*** Initializing new MinHeap ... done." )
    print( "*** Adding values 7, 42, 999, 0, 212, 512, 21 ... done." )

    test = MinHeap()

    test.insert( 7   )
    test.insert( 42  )
    test.insert( 999 )
    test.insert( 0   )
    test.insert( 212 )
    test.insert( 512 )
    test.insert( 21  )

    print( "   ### Current MinHeap: " )
    test.println()
    print( "\n   ### Number of Nodes: " + str( test.getSize() ))

    print( "\n*** Performing \"deleteMin\" ... done." )
    test.deleteMin()

    print( "   ### Current MinHeap: " )
    test.println()
    print( "\n   ### Number of Nodes: " + str( test.getSize() ))


if __name__ == '__main__':
    main()

