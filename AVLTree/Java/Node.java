/**
 * Class representing a single Node of an AVL tree.
 * A Node contains a value that is also the insertion
 * key and references to its children and parent Nodes.
 *
 * @author Thomas Lang
 * @version 2014/09/02
 */
public class Node<T extends Comparable<T>> {

    private T value;
    private Node<T> left, right, parent;

    /**
     * Creates a new Node containing
     * the value <code>value</code>.
     *
     * @param value The value saved in the Node.
     */
    public Node( T value ){
        this.value = value;
	    left       = null;
    	right      = null;
	    parent     = null;
    }

    /**
     * Returns the saved value.
     *
     * @return The saved value in the Node.
     */
    public T getValue(){ return value; }

    /**
     * Returns the balance of the Node.
     *
     * @return The balance of the Node.
     */
    public int getBalance(){
        if(( left == null ) && ( right == null ))
	        return 0;
	    return ( height( right ) - height( left ) );
    }

    /**
     * Helper function for calculating the height 
     * of the Node <code>node</code>.
     *
     * @param  node The node to determine its height.
     * @return The height of <code>node</code>.
     */
    private int height( Node<T> node ){
	    if( node == null )
	        return 0;
	    return 1 + Math.max( height( node.getLeft() ), height( node.getRight() ));
    }

    /**
     * Returns the reference to the left child.
     *
     * @return The reference to the left child.
     */
    public Node<T> getLeft(){ return left; }

    /**
     * Returns the reference to the right child.
     *
     * @return The reference to the right child.
     */
    public Node<T> getRight(){ return right; }

    /**
     * Returns the reference to the parent Node.
     *
     * @return The reference to the parent Node.
     */
    public Node<T> getParent(){ return parent; }

    /**
     * Sets the value of the Node.
     *
     * @param value The new value to store.
     */
    public void setValue( T value ){
        this.value = value;
    }

    /**
     * Sets the left reference to <code>node</code>.
     *
     * @param node The new left reference.
     */
    public void setLeft( Node<T> node ){
        left = node;
    }

    /**
     * Sets the right reference to <code>node</code>.
     *
     * @param node The new right reference.
     */
    public void setRight( Node<T> node ){
        right = node;
    }

    /**
     * Sets the parent reference to <code>node</code>.
     *
     * @param node The new parent reference.
     */
    public void setParent( Node<T> node ){
        parent = node;
    }

    /**
     * Returns an appropriate String representation
     * of the calling Node.
     *
     * @return A String representation of the Node.
     */
    public String toString(){
        return ("{" + value + "}");
    }
}
