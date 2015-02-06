#!/usr/env python

import queue

class Node:
    """This class represents a single 
    Node of a binary search tree."""

    def __init__( self, value ):
        """Constructor, initializes the attributes."""
        self.value  = value
        self.bfsnum = 0
        self.left   = None
        self.right  = None
        self.parent = None

    def __iter__(self):
        """Making a node iterable by simply yielding its value.
        
        @return The value of the node. """
        yield self.value

    def left(self):
        """Returns the reference to the left child of this node.
        
        @return The ref to the left child."""
        return self.left

    def right(self):
        """Returns the reference to the right child of this node.
        
        @return The ref to the right child."""
        return self.right

    def __str__( self ):
        """String representation of a single Node."""
        return str(self.value)


class BinarySearchTree:
    """This class represents a binary
    search tree and gives some methods/
    functions to operate with it."""

    def __init__( self ):
        """Constructor, initializes the root node to NULL."""
        self.root = None
        self.size = 0

    ##########################################################

    def isEmpty( self ):
        """Checks whether the tree is empty or not."""
        return ( self.root is None )

    def getSize(self):
        """Gets the number of elements in the tree."""
        return self.size

    def contains(self, element):
        """Checks if element is contained in tree."""
        return self.__findNode(element) is not None

    def getAll(self):
        """Returns a list of all elements in the tree by 
           traversing the tree with breadth-first-search.
           
        @return A list of all elements in the tree."""

        if self.isEmpty():
            return None

        l = []
        q = queue.LifoQueue()
        l += self.root
        q.put(self.root)

        while not q.empty():
            n = q.get()

            if n is not None:
                le = n.left
                ri = n.right

                if le is not None:
                    l += le
                    q.put(le)

                if ri is not None:
                    l += ri
                    q.put(ri)

        return l


    ###########################################################

    def add( self, value ):
        """Adds a new Node containing the passed value
        to the tree by following the rules of a binary
        search tree.
        
        @param  value  The value to insert.
        @return True if the value was inserted successfully, False otherwise
        """
        if self.isEmpty():
            self.root = Node( value )
            self.size += 1
            return True
        else:
            return self.__insertIntoTree( self.root, value )

    def __insertIntoTree( self, node, value ):
        """Does the main inserting. Follows this rule:
                If the value is less than the current node,
                it goes into the left subtree. If it's greater
                then it steps into the right subtree.
                If none of the above things match, it will
                completely ignore it, because here are no 
                duplicates allowed.
        
        @param  node   Current node.
        @param  value  Value to insert.
            """
        if node.value > value:
            if node.left is not None:
                self.__insertIntoTree( node.left, value )
            else:
                node.left = Node( value )
                node.left.parent = node
                self.size += 1
                return True
        elif node.value < value:
            if node.right is not None:
                self.__insertIntoTree( node.right, value )
            else:
                node.right = Node( value )
                node.right.parent = node
                self.size += 1
                return True
        else:
            return False # no duplicates allowed

    ###########################################################

    def preorder( self ):
        """Traverses the tree in preorder direction."""
        if self.root is not None:
            self.__preorder( self.root )
        print


    def __preorder( self, node ):
        """The main traversing, goes recursively.
        
        @param  node  Current node.
        """
        print( node )
        if node.left is not None:
            self.__preorder( node.left )
        if node.right is not None:
            self.__preorder( node.right )

    ###########################################################

    def inorder( self ):
        """Traverses the tree in inorder direction."""
        if self.root is not None:
            self.__inorder( self.root )
        print


    def __inorder( self, node ):
        """The main traversing, goes recursively
        
        @param  node  Current node.
        """
        if node.left is not None:
            self.__inorder( node.left )
        print( node )
        if node.right is not None:
            self.__inorder( node.right )

    ###########################################################

    def postorder( self ):
        """Traverses the tree in postorder direction."""
        if self.root is not None:
            self.__postorder( self.root )
        print


    def __postorder( self, node ):
        """The main traversing, goes recursively.
        
        @param  node  Current node.
        """
        if node.left is not None:
            self.__postorder( node.left )
        if node.right is not None:
            self.__postorder( node.right )
        print( node )

    ###########################################################

    def breadthfirst( self ):
        """Traverses the tree in breadthfirst-direction.
        Normally we would use a Queue, but instead of 
        importing it we just simulate it with a list."""
        queue = []
        queue.append( self.root )
        bfscount = 1
        self.root.bfsnum = bfscount
        while( len( queue ) != 0 ):
            v = queue.pop()
            print( v )
            bfscount += 1; v.bfsnum = bfscount
            if v.left is not None:
                queue.insert( 0, v.left )
            if v.right is not None:
                queue.insert( 0, v.right )
        print

    ###########################################################

    def delete( self, value ):
        """Deletes the Node containing 'value' from the tree.
        
        @param  value  Content of the node-to-delete
        @return True if the element was deleted successfully, False otherwise
        """
        if self.isEmpty():
            return False
        
        nodeToDelete = self.__findNode( value )
        
        if nodeToDelete is not None:
            if nodeToDelete.value == self.root.value:
                self.__deleteInternalNode( self.root )
            elif ( nodeToDelete.left is not None ) and ( nodeToDelete.right is not None ):
                self.__deleteInternalNode( nodeToDelete )
            else:
                self.__deleteNode( nodeToDelete )

            self.size -= 1
            return True
        else:
            return False


    def __deleteInternalNode( self, node ):
        """Deletes an internal node by swapping it's value
        with the next-less node and then deleting that node.
        
        @param  node  Current node
        """
        maxOfMin = self.__findMaxOfMinNode( node )
        node.value = maxOfMin.value
        self.__deleteNode( maxOfMin )


    def __deleteNode( self, node ):
        """Deletes any other node and looks, if it's a left 
        or a right child (to guarantee after the process that
        we also have a search tree.
        
        @param  node  Current node.
        """
        if ( node.left is None ) and ( node.right is None ):
            if node.parent.right.value == node.value:
                node.parent.right = None
                node.parent = None
            else:
                node.parent.left = None
                node.parent = None
        elif node.left is not None:
            if node.parent.right.value == node.value:
                node.parent.right = node.left
                node.left.parent = node.parent
                node.parent = None
            else:
                node.parent.left = node.left
                node.left.parent = node.parent
                node.parent = None
        else:
            if node.parent.right.value == node.value:
                node.parent.left = node.right
                node.right.parent = node.parent
                node.parent = None
            else:
                node.parent.right = node.right
                node.right.parent = node.parent
                node.parent = None


    def __findNode( self, element ):
        """Finds a specific node containing the value
        'element' in the tree."""
        if self.isEmpty():
            print( "Error! No elements in empty tree." )
        else:
            return self.__findNodeRec( self.root, element )
        

    def __findNodeRec( self, node, element ):
        """Does the main finding, goes recursively 
        through the tree"""
        if node.value == element:
            return node
        elif node.value > element:
            return self.__findNodeRec( node.left, element )
        else:
            return self.__findNodeRec( node.right, element )


    def __findMaxOfMinNode( self, node ):
        """Finds the maximum node of the left
        subtree of node, which is needed for
        deleting.
        
        @param  node  Current node
        """
        if node.left is not None:
            leftChild = node.left
            while leftChild.right is not None:
                leftChild = leftChild.right
        return leftChild 


    ###########################################################

    def __str__( self ):
        """String representation of a binary search tree."""
        output = ""
        
        if self.isEmpty():
            output += "(empty tree)"
        else:
            for item in self.getAll():
                output += str(item)
                output += ", "

            output = output[0:len(output) - 2:]
        
        return output

################################################################

def main():
    """Main method, does some testing."""
    print( "*** Initializing new binary search tree ..."    )
    print( "*** Adding values: 5, 7, 9, 1, 4, 0, 6, 2, 42 " )
    t = BinarySearchTree()
    t.add( 5 ); t.add( 7 ); t.add( 9 ); t.add( 1 );
    t.add( 4 ); t.add( 0 ); t.add( 6 ); t.add( 2 ); t.add( 42 );
    print( "\n  ### Actual tree: \n" )
    print( t )
    print( "Preorder-traversal:" )
    t.preorder()
    print( "Inorder-traversal: ")
    t.inorder()
    print( "Postorder-traversal: ")
    t.postorder()
    print( "Breadth-first-traversal: " )
    t.breadthfirst()
    print; print
    print( "Deleting Node containing \"42\" ..." )
    t.delete( 42 )
    print( "\n  ### Actual tree: \n" )
    print( t )
    print; print
    print( "Deleting Node containing \"5\" ..." )
    t.delete( 5 )
    print( "\n  ### Actual tree: \n" )
    print( t )

if __name__ == "__main__":
    main()
