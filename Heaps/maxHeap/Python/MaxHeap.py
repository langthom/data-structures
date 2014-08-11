
from Heap import *

class MaxHeap( Heap ):
    """Specialized form of the abstract Heap.
       This one follows the special rule, that
       a Node is always greater (by value) than
       its children, there is still no connection
       between its children. This has the 
       advantage that the root of this tree is
       ALWAYS THE GLOBAL MAXIMUM of the entire Heap."""

    def __init__( self ):
        """Creates a new but empty MaxHeap."""
        super( MaxHeap, self ).__init__()


    def insert( self, value ):
        """Inserts a new Node containing the passed
           value to the tree and then performs a
           bubbleUp (if necessary).
           
        @param  value  The value to add.    """
        node = Node( value )
        super( MaxHeap, self ).insert( node )
        self.__bubbleUp( node )


    def __bubbleUp( self, node ):
        """Performs a 'bubbleUp' operation.
           That means that it swaps a node 
           with its parent Node if the 
           condition mentioned above is
           violated.
           
        @param  node  The node to bubbleUp"""
        if node.parent is None:
            return
        if node.value > node.parent.value:
            self.__swapNodes( node, node.parent )
            self.__bubbleUp( node.parent )


    def __swapNodes( self, child, parent ):
        """Helper method, swaps the two
           passed Nodes by value only.
           
        @param  child  The child Node.
        @param  parent The parent Node. """
        childValue = child.value
        child.setValue( parent.value )
        parent.setValue( childValue )


    def deleteMax( self ):
        """Deletes the maximum Node that is
           the root of the Heap (see 
           class docstring ).          """
        last = self.getLastNode()
        self.root.setValue( last.value )

        if last.parent.left == last:
            last.parent.setLeft( None )
            last.setParent( None )
        else:
            last.parent.setRight( None )
            last.setParent( None )

        self.size -= 1
        self.__bubbleDown( self.root )


    def __bubbleDown( self, node ):
        """Performs a 'bubbleDown' operation, so it
           corrects any misstandings in the Heap
           caused by the 'deleteMax' operation. 
           
        @param  node  The node to bubbleDown.    """
        if node.left is None and node.right is None:
            return

        if node.left is not None and node.right is not None:
            if node.value < node.left.value:
                self.__swapNodes( node, node.left )
                self.__bubbleDown( node.left )
            elif node.value < node.right.value:
                self.__swapNodes( node, node.right )
                self.__bubbleDown( node.right )

        if node.left is not None and node.value < node.left.value:
            self.__swapNodes( node, node.left )
            self.__bubbleDown( node.left )

        if node.right is not None and node.value < node.right.value:
            self.__swapNodes( node, node.right )
            self.__bubbleDown( node.right )

