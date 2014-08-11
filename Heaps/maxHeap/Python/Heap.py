
from Node import *

class Heap( object ):
    """Class representing a typical (abstract) Heap.
       This is principally a binary search tree that
       follows special rules. These differ on the 
       inheritated forms (MinHeap / MaxHeap), but a
       general rule is that new Nodes are inserted 
       strictly from left to right, there is NO 
       NUMERICAL connection between a left and a right
       Node."""

    def __init__( self ):
        """Creates a new but empty Heap."""
        self.root = None
        self.last = None  # pointer to last node added
        self.size = 0

    ################################################################################

    def isEmpty( self ):
        """Checks whether the Heap is empty or not.
        
        @return True if Heap is empty, False otherwise."""
        return ( self.root is None )

    ################################################################################

    def getSize( self ):
        """Returns the size (the number of 
           Nodes) of the Heap.
           
        @return The size of the Heap."""
        return self.size

    ################################################################################

    def insert( self, node ):
        """Inserts a new Node into the tree by
           following the rule mentioned above.
           
        @param  node  The node to insert.      """
        if self.isEmpty():
            self.root = node
            self.last = node
        else:
            self.__insert( self.root, node )
        self.size += 1


    def __insert( self, node, newNode ):
        """Helper method for inserting a
           Node recursively.
           
        @param  node     Current Node (for recursion)
        @param  newNode  The new Node to insert.     """
        if node is None:
            node = newNode
            self.last = newNode
        elif node.left is None:
            node.setLeft( newNode )
            newNode.setParent( node )
            self.last = newNode
        elif node.right is None:
            node.setRight( newNode )
            newNode.setParent( node )
            self.last = newNode
        elif self.__isFullTree( node ) and self.__isFullTree( node.left ):
            self.__insert( node.right, newNode )
        elif self.__isFullTree( node ):
            self.__insert( node.left, newNode )


    def __isFullTree( self, node ):
        """Helper function for checking whether the passed
           Node has both a left and a right child.
           
        @param  node  The node to check.
        @return True if the node has both a left and a right
                child, False otherwise"""
        if node is not None and node.left is not None and node.right is not None:
            return True
        return False

    ################################################################################
    
    def getLastNode( self ):
        """Returns a reference to the last Node added.
        
        @return A pointer to the last Node added.     """
        if self.isEmpty():
            raise LookupError( "Error! No elements in Heap." )
        else:
            return self.last

    ################################################################################

    def println( self ):
        """Prints the calling Heap to stdout."""
        if self.isEmpty():
            print( "(empty heap)" )
        else:
            print( "Root: " + str( self.root ) )
            self.__printRec( self.root )


    def __printRec( self, node ):
        """Helper function for printing.
        
        @param  node  Current node (for recursion). """
        if node.left is not None:
            print( "Left: " + str( node.left ))
        if node.right is not None:
            print( "Right: " + str( node.right ))
        if node.left is not None:
            self.__printRec( node.left )
        if node.right is not None:
            self.__printRec( node.right )

