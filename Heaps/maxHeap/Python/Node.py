
class Node:
    """Class representing a single Node.
       A Node saves a value and references
       to its left and right child and to 
       its parent node."""
    
    def __init__( self, value ):
        """Creates a new Node.
           It saves the passed value and
           initializes the references to
           children and parent Node to None.
           
        @param  vale  The value to save."""
        self.value  = value
        self.left   = None
        self.right  = None
        self.parent = None

    @property
    def value( self ):
        """Returns the saved value in a Node.
        
        @return The saved value."""
        return self.value

    @property
    def left( self ):
        """Returns the reference pointing to 
           the left child of the calling Node.
           
        @return The left pointer."""
        return self.left

    @property
    def right( self ):
        """Returns the reference pointing to
           the right child of the calling Node.
           
        @return The right pointer."""
        return self.right

    @property
    def parent( self ):
        """Returns the reference pointing to 
           the parent Node of the calling Node.
           
        @return The parent pointer."""
        return self.parent

    def setValue( self, value ):
        """Sets the saved value to the passed one.
        
        @param  value  The new value to save."""
        self.value = value

    def setLeft( self, node ):
        """Sets the left reference to the passed one.
        
        @param  node  The new left reference."""
        self.left = node

    def setRight( self, node ):
        """Sets the right reference to the passed one.
        
        @param  node  The new right reference."""
        self.right = node

    def setParent( self, node ):
        """Sets the right reference to the passed one.
        
        @param  node  The new parent reference."""
        self.parent = node

    def __str__( self ):
        """Returns an appropriate String representation
           of the calling node.
           
        @return A String representation of the Node."""
        return "{" + str( self.value ) + "}"
