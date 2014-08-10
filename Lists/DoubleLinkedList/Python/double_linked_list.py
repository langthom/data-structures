#!/usr/bin/env python

class Node:
    """Class representing a single Node.
       A Node saves a value and a reference
       to its successor."""

    def __init__( self, value ):
        """Constructor, saves the
           passed value.
           
        @param  value  The value to save."""
        self.value    = value
        self.nextNode = None
        self.prevNode = None

    
    def getValue( self ):
        """Returns the value saved in the Node.
        
        @return The value saved in the Node."""
        return self.value


    def getPrev( self ):
        """Returns the previous reference.
        
        @return The prev pointer."""
        return self.prevNode


    def getNext( self ):
        """Returns the next reference.
        
        @return The next pointer."""
        return self.nextNode


    def setPrev( self, node ):
        """Sets the prev reference to node.
        
        @param  node  The new prev reference."""
        self.prevNode = node


    def setNext( self, node ):
        """Sets the next reference to node.
        
        @param  node  The new next reference."""
        self.nextNode = node


    def __str__( self ):
        """Returns a String representation of a Node.
        
        @return A String representation of a Node."""
        return ("{" + str(self.value) + "}")


class List:
    """Class representing a single linked list."""

    def __init__(self):
        """Creates a new but empty list."""
        self.front = None
        self.rear  = None
        self.size  = 0


    def isEmpty( self ):
        """Checks if list is empty.
        
        @return true if the list is
                empty, false otherwise """
        return ( self.front is None )


    def append( self, value ):
        """Appends a new Node containing the
        value "value" to the list.
        
        @param  value  The value to add."""
        node = Node( value )
        if self.isEmpty():
            self.front = node
        else:
            self.rear.setNext( node )
            node.setPrev( self.rear )
        self.rear = node
        self.size += 1


    def delete( self, value ):
        """Deletes the Node containing
           the value "value" from the list.
           
        @param  value  The value of the Node to delete."""
        if self.isEmpty():
            raise LookupError( "Error! Cannot delete from empty list." )
        elif self.front.getValue() == value:
            self.front = self.front.getNext()
        elif self.rear.getValue() == value:
            tmp = self.rear.getPrev()
            self.rear.setPrev( None )
            tmp.setNext( None )
        else:
            tmp = self.front
            while tmp is not None:
                if tmp.getValue() == value:
                    tmp.getPrev().setNext( tmp.getNext() )
                    tmp.getNext().setPrev( tmp.getPrev() )
                    break
                tmp = tmp.getNext()
            else:
                raise LookupError( "Error! Element not found in list." )
        self.size -= 1


    def __str__( self ):
        """String representation of a list.
        
        @return A String representation of a list."""
        out = "[ "
        tmp = self.front
        while tmp is not None:
            out += str(tmp) + " "
            tmp = tmp.getNext()
        out += "]"
        return out


def main():
    print( "*** Creating new list ... done." )
    l = List()

    print( "*** Appending values 7, 42, 0, 999 ... done." )
    l.append( 7   )
    l.append( 42  )
    l.append( 0   )
    l.append( 999 )

    print( "   ### Current list: " )
    print( l )

    print( "*** Deleting '0' ... done." )
    l.delete( 0 )

    print( "   ### Current list: " )
    print( l )


if __name__ == '__main__':
    main()
