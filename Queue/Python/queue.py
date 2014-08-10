#!/usr/bin/env python

from double_linked_list import *

class Queue:
    """Class representing a Queue which is
       simply a double linked list. """

    def __init__( self ):
        """Creates a new but empty Queue."""
        self.dlist = List()


    def isEmpty( self ):
        """Checks wheter the Queue is empty or not.
        
        @return true if the Queue is empty,
                false otherwise.  
        """
        return self.dlist.isEmpty()


    def enqueue( self, value ):
        """Adds a new node containing "value" 
        at the front of the Queue.
        
        @param  value  The new value to add."""
        node = Node( value )
        self.dlist.addFront( node )


    def dequeue( self ):
        """Deletes the last element of the Queue."""
        self.dlist.pop()


    def __str__( self ):
        """Returns a String representation of the Queue.
        
        @return A String representation of the Queue.
        """
        return self.dlist.__str__()


def main():
    print( "*** Creating new Queue ... done." )
    q = Queue()

    print( "*** Enqueueing values 0, 7, 42, 999 ... done." )
    q.enqueue( 0   )
    q.enqueue( 7   )
    q.enqueue( 42  )
    q.enqueue( 999 )

    print( "   ### Current Queue: " )
    print( q )

    print( "*** Dequeueing ... done." )
    q.dequeue()

    print( "   ### Current Queue: " )
    print( q )



if __name__ == '__main__':
    main()

