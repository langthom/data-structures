// BinarySearchTree.java - Implementation of a binary search tree

/**
 * Implementation of a generic binary search tree in Java.
 *
 * @author  Thomas Lang
 * @version 0.1
 */
public class BinarySearchTree<T extends Comparable<T>>{

    /**
     * Class representing a single Node of the tree.
     *
     * @author  Thomas Lang
     * @version 0.1
     */
    private class Node<T extends Comparable<T>>{
        private T value;
        private Node<T> left;
        private Node<T> right;
        private Node<T> parent;

        /**
         * Constructor, saves the passed generic value.
         *
         * @param  value   The value to save
         */
        private Node( T value ){
            this.value = value;
            left = right = parent = null;
        }

        /**
         * String representation of a node.
         *
         * @return  String representation
         */
        public String toString(){
            return ( "{" + value + "}" );
        }
    }


    // root node of the whole tree, we only have one
    // root node in a BINARY search tree
    Node<T> root;

    /**
     * Constructor, initializes the tree to null.
     */
    public BinarySearchTree(){ root = null; }

    /**
     * Adds a new node containing the value "value".
     *
     * @param  value   Value to insert.
     */
    public void add( T value ){
        if( this.isEmpty() ){
            root = new Node<T>( value );
        }else{
            insertInTree( root, value );
        }
    }

    /**
     * Inserts a new node containing the value "value"
     * to the binary search tree by using it's property
     * that all elements with a smaller value than the
     * actual node are left of this, the ones greater
     * then the actual node are right of it.
     *
     * @param  node   Actual node, for traversing
     * @param  value  Value to insert.
     */
    private void insertInTree( Node<T> node, T value ){
        if( value.compareTo( node.value ) < 0 ){
            if( node.left != null ){
                insertInTree( node.left, value );
            }else{
                node.left = new Node<T>( value );
                node.left.parent = node;
            }
        }else if( value.compareTo( node.value ) > 0 ){
            if( node.right != null )
                insertInTree( node.right, value );
            else{
                node.right = new Node<T>( value );
                node.right.parent = node;
            }
        }else{
            return;     // no duplicates allowed
        }
    }

    /**
     * Traverses the tree in Preorder.
     */
    public void traversePreorder(){
        if( root != null )
            traversePreorderRec( root );
        System.out.println();
    }

    /**
     * Traversing by first printing the current
     * node, then stepping through the left and
     * lastly through the right subtree.
     *
     * @param  node  Current node.
     */
    private void traversePreorderRec( Node<T> node ){
        System.out.print( node );
        if( node.left != null )
            traversePreorderRec( node.left );
        if( node.right != null )
            traversePreorderRec( node.right );
    }

    /**
     * Traverses the tree in Inorder direction.
     */
    public void traverseInorder(){
        if( root != null )
            traverseInorderRec( root );
        System.out.println();
    }

    /**
     * Traversing by first stepping throug the left
     * subtree of the current node, then printing it
     * and lastly stepping through the right subtree.
     *
     * @param  node  Current node.
     */
    private void traverseInorderRec( Node<T> node ){
        if( node.left != null )
            traverseInorderRec( node.left );
        System.out.print( node );
        if( node.right != null )
            traverseInorderRec( node.right );
    }

    /**
     * Traverses the tree in Postorder direction.
     */
    public void traversePostorder(){
        if( root != null )
            traversePostorderRec( root );
        System.out.println();
    }

    /**
     * Traversing by first stepping through the left 
     * and the right subtree of the current node and
     * lastly printing the current node.
     *
     * @param  node  Current node.
     */
    private void traversePostorderRec( Node<T> node ){
        if( node.left != null )
            traversePostorderRec( node.left );
        if( node.right != null )
            traversePostorderRec( node.right );
        System.out.print( node );
    }

    /**
     * Deletes the node containing the value "element" from the tree.
     *
     * @param  element  Value of the node-to-delete.
     */
    public void delete( T element ){
        if( isEmpty() )
            System.err.println( "Error! Cannot delete from empty Tree." );
        Node<T> nodeToDelete = findNode( element );
        if( nodeToDelete != null ){
            if( nodeToDelete.value.compareTo( root.value ) == 0 )
                deleteRoot();
            else if( nodeToDelete.left != null && nodeToDelete.right != null )
                deleteInternalNode( nodeToDelete );
            else 
                deleteNode( nodeToDelete );
        }else{
            System.err.println( "Error! Element not found" );
        }
    }

    /**
     * Deletes the root element.
     */
    private void deleteRoot(){
        deleteInternalNode( root );
    }

    /**
     * Deletes an internal node by swapping the 
     * node-to-delete with the next-smaller node
     * (which has either none or one child), and
     * then deleting this node.
     *
     * @param  node  Node-to-delete.
     */
    private void deleteInternalNode( Node<T> node ){
        // deleting internal node by swapping with
        // maximum of left child-subtree
        Node<T> maxOfMin  = findMaxOfMinNode( node );
        node.value = maxOfMin.value;
        deleteNode( maxOfMin );
    }

    /**
     * Deletes a node that has either one or no child.
     *
     * @param  node  The node to delete.
     */
    private void deleteNode( Node<T> node ){
        if(( node.left == null ) && ( node.right == null )){
            if( node.parent.right.value.compareTo( node.value ) == 0 ){
                node.parent.right = null;
                node.parent = null;
            }else{
                node.parent.left = null;
                node.parent = null;
            }
        }else if( node.left != null ){
            if( node.parent.right.value.compareTo( node.value ) == 0 ){
                node.parent.right = node.left;
                node.left.parent = node.parent;
                node.parent = null;
            }else{
                node.parent.left = node.left;
                node.left.parent = node.parent;
                node.parent = null;
            }
        }else{
            if( node.parent.right.value.compareTo( node.value ) == 0 ){
                node.parent.left = node.right;
                node.right.parent = node.parent;
                node.parent = null;
            }else{
                node.parent.right = node.right;
                node.right.parent = node.parent;
                node.parent = null;
            }
        }
    }

    /**
     * Finds a node containing the value "element" in the tree.
     *
     * @param   element  Value of the Node-to-Find
     * @return  The node with the value "element", null otherwise.
     */
    public Node<T> findNode( T element ){
        if( isEmpty() ) 
            System.err.println( "Error! No elements in Empty Tree." );
        else
            return findNodeRec( root, element );
        return null;
    }

    /**
     * Finds the node containing the value "element" by traversing
     * through the tree.
     *
     * @param  node     Current node.
     * @param  element  Value of the node-to-find.
     * @return The node, if found, null otherwise.
     */
    private Node<T> findNodeRec( Node<T> node, T element ){
        if( node.value.equals( element ) )
            return node;
        else if( node.value.compareTo( element ) < 0 )
            findNodeRec( node.left, element );
        else
            findNodeRec( node.right, element );
        return null;
    }

    /**
     * Finds the next-smaller node in the tree.
     *
     * @param  node  Current node.
     * @return next-smaller node in the tree.
     */
    public Node<T> findMaxOfMinNode( Node<T> node ){
        Node<T> leftChild = null;
        if( node.left != null ){
            leftChild = node.left;
            while( leftChild.right != null ){
                leftChild = leftChild.right;
            }
        }
        return leftChild;
    }

    /**
     * Checks whether the tree is empty or not.
     *
     * @return true if tree is empty, false otherwise
     */
    public boolean isEmpty(){ return ( root == null ); }

    /**
     * Prints the calling tree.
     */
    public void printTree(){
        if( isEmpty() )
            System.out.println( "( empty tree )" );
        else{
            System.out.println( "Root: " + root );
            printTreeRec( root );
        }
    }

    /**
     * Prints the tree recursively.
     *
     * @param  node  The current node.
     */
    private void printTreeRec( Node<T> node ){
        if( node.left != null )
            System.out.println( "Left: " + node.left );
        if( node.right != null )
            System.out.println( "Right: " + node.right );
        if( node.left != null )
            printTreeRec( node.left );
        if( node.right != null )
            printTreeRec( node.right );
    }
}
