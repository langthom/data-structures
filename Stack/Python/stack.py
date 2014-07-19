#!/usr/env/python

class Node:
    """Representation of a single Node.
    A node saves a value and a reference
    to its successor."""

    def __init__( self, value ):
        """Constructor for a new Node.
        
        @param  value  The value to save.
        """
        self.value     = value
        self.nextNode  = None

    def getValue( self ):
        """Returns the saved value.
        
        @return The saved value.
        """
        return self.value

    def setValue( self, value ):
        """Sets the saved value to the
        passed parameter.
        
        @param  value  The value to save.
        """
        self.value = value

    def getNext( self ):
        """Receives "next" reference.
        
        @return  The "next" pointer.
        """
        return self.nextNode

    def setNext( self, node ):
        """Sets the "next" reference.
        
        @param  node  The new "next" pointer.
        """
        self.nextNode = node

    def __str__( self ):
        """Returns a nice string representation
        of a single Node-object.
        
        @return String representation of a Node.
        """
        return ("{" + str(self.value) + "}")


class Stack:
    """Implementation of a typical Stack. So a Stack
    is a data-structure that allows access to the most
    top element, the stack pointer, only."""

    def __init__( self ):
        """Constructor for a new, empty Stack."""
        self.sp   = None
        self.size = 0

    def isEmpty( self ):
        """Checks, whether a Stack is empty or not.
        
        @return True if the Stack is empty, False otherwise.
        """
        return (self.sp is None)

    def getSize( self ):
        """Returns the size of the Stack.
        
        @return The size of the Stack.
        """
        return self.size

    def push( self, value ):
        """Pushes a new Node containing the passed parameter
        to the Stack.
        
        @param  value  The value of the new Node.
        """
        node = Node( value )
        if self.isEmpty():
            self.sp = node
        else:
            node.setNext( self.sp )
            self.sp = node 
        self.size += 1

    def pop( self ):
        """Performs the "pop" operation on a Stack,
        deletes top Node and returns it.
        
        @return The top Node.
        """
        if self.isEmpty():
            raise Exception( "Error! Cannot pop from empty Stack." )
        topNode    = self.sp
        self.sp    = self.sp.getNext()
        self.size -= 1
        return topNode

    def top( self ):
        """Returns the top element WITHOUT deleting it."""
        if self.isEmpty():
            raise Exception( "Error! No elements in empty Stack." )
        return self.sp

    def __str__( self ):
        """Returns a String representation of a Stack.
        
        @return A String representation of a Stack.
        """
        out = ""
        if self.isEmpty():
            return "(empty stack)"
        else:
            out += "---> " + str(self.sp) + "\n"
            top = self.sp.getNext()
            while top is not None:
                out += "     " + str(top) + "\n"
                top = top.getNext()
            out += "\n"
        return out



def main():
    """Main-method, does some testing."""
    print( "*** Initializing new Stack ... done." )
    stack = Stack()

    print( "*** Pushing values 7, 42, 24, and 999 to Stack." )
    stack.push( 7   )
    stack.push( 42  )
    stack.push( 24  )
    stack.push( 999 )
    
    print( "   ### Current stack:")
    print( stack )

    print( "   ### Top element: " + str(stack.top()) )
    print( "\n*** Performing pop operation ... done " )
    stack.pop()
    
    print( "   ### Current stack:" )
    print( stack )


if __name__ == '__main__':
    main()
