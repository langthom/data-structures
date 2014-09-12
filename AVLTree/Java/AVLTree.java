/**
 * Representation of an AVL tree.
 * An AVL tree is a specialization of a binary
 * search tree which follows the special rule,
 * that it re-balances itself after every insertion
 * and deletion.
 * This brings the advantage, that we have fast
 * lookup times every time, so the tree CANNOT
 * become dengerate.
 *
 * @author Thomas Lang
 * @version 2014/09/02
 */
public class AVLTree <T extends Comparable<T>> 
    extends BinarySearchTree<T> {

    /**
     * Creates a new but empty AVL tree.
     */
    public AVLTree(){
        super();
    }

    /**
     * Inserts a new node containing the
     * value <code>value</code> to the tree
     * and performs a re-balancing if necessary.
     *
     * @param value The new value to insert.
     */
    public void insert( T value ){
        Node<T> node = new Node<T>( value );
        super.insert( node );
        balance( node );
    }

    /**
     * Performs a re-balancing of the AVL tree
     * if the newly inserted <code>node</code>
     * disturbes the balancing.
     *
     * @param node The newly inserted node.
     */
    private void balance( Node<T> node ){
        int bal  = node.getBalance();
        int lbal = 0, rbal = 0;

        if( node.getLeft() != null )
            lbal = node.getLeft().getBalance();
        if( node.getRight() != null )
            rbal = node.getRight().getBalance();

    	if( bal == 2 ){
	        if( rbal == 1 )
		        rotateLeftLeft( node );
	        else if( rbal == -1 )
		    rotateRightLeft( node );
	    }else if( bal == -2 ){
	        if( lbal == -1 )
		        rotateRightRight( node );
	        else if( lbal == 1 )
		        rotateLeftRight( node );
	    }else if( node.getParent() != null ){
	        balance( node.getParent() );
	    }
    }

    /**
     * Performs a left rotation.
     *
     * @param node The node to rotate about.
     */
    private void rotateLeftLeft( Node<T> node ){
	    Node<T> r = node.getRight();
	    if( node == root )
	        root = r;
	    r.setParent( node.getParent() );
	    node.setParent( r );
	    node.setRight( r.getLeft() );
	    r.setLeft( node );
    }

    /**
     * Performs a right rotation.
     *
     * @param node The node to rotate about.
     */
    private void rotateRightRight( Node<T> node ){
        Node<T> l = node.getLeft();
	    if( node == root )
	        root = l;
	    l.setParent( node.getParent() );
	    node.setParent( l );
	    node.setLeft( l.getRight() );
	    l.setRight( node );
    }

    /**
     * Performs a left-right double rotation.
     *
     * @param node The node to rotate about.
     */
    private void rotateLeftRight( Node<T> node ){
        rotateLeftLeft( node.getLeft() );
        rotateRightRight( node );
    }

    /**
     * Performs a right-left double rotation.
     *
     * @param node The node to rotate around.
     */
    private void rotateRightLeft( Node<T> node ){
        rotateRightRight( node.getRight() );
        rotateLeftLeft( node );
    }
}
