/**
 * Representation of a typical generic binary search tree.
 * Such a tree contains the root node and insertes/removes
 * elements by searching the node according to its key.
 *
 * @author Thomas Lang
 * @version 2014/09/02
 */
public class BinarySearchTree<T extends Comparable<T>> {

    protected Node<T> root;
    protected int size; 

    /**
     * Creates a new but empty binary search tree.
     */
    public BinarySearchTree(){
        root   = null;
        size   = 0;
    }

    /**
     * Checks whether the tree is empty or not.
     *
     * @return {@code true} if the tree is empty,
     *         {@code false} otherwise.
     */
    public boolean isEmpty(){ return ( root == null ); }

    /**
     * Returns the number of elements in the tree.
     *
     * @return The size of the tree.
     */
    public int size(){ return size; }

    /**
     * Returns the height of the tree.
     *
     * @return The height of the tree.
     */
    public int height(){
	    if( isEmpty() )
	        return 0;
	    return height( root )-1;
    }

    /**
     * Recursive helper function to
     * calculate the height of the tree.
     *
     * @param  node The current Node.
     * @return The height of the tree.
     */
    private int height( Node<T> node ){
	    if( node == null )
	        return 0;
	    return 1+Math.max( height(node.getLeft()), height(node.getRight()));
    }

    /**
     * Inserts the Node <code>node</code>
     * to the binary search tree.
     *
     * @param node The new Node to insert.
     */
    public void insert( Node<T> node ){
        if( isEmpty() ){
            root = node;
        }else{
            insert( root, node );
        }
        size++;
    }

    /**
     * Recursive helper function for 
     * inserting a new Node.
     */
    private void insert( Node<T> cur, Node<T> node ){
        if( node.getValue().compareTo( cur.getValue() ) < 0 ){
            if( cur.getLeft() == null ){
                cur.setLeft( node );
                node.setParent( cur );
            }else{
                insert( cur.getLeft(), node );
            }
        }else if( node.getValue().compareTo( cur.getValue() ) > 0 ){
            if( cur.getRight() == null ){
                cur.setRight( node );
                node.setParent( cur );
            }else{
                insert( cur.getRight(), node );
            }
        }else{
            System.out.println( "No duplicates allowed, node" +
                    " won't be inserted." );
        }
    }

    /**
     * Prints the tree to stdout.
     */
    public void print(){
        if( isEmpty() )
            System.out.println( "(empty tree)" );
        else{
            System.out.println( "Root: " + root );
            print( root );
        }
    }

    /**
     * Recursive helper function for 
     * printing the tree preorder-way.
     *
     * @param node The current node.
     */
    private void print( Node<T> node ){
        if( node.getLeft() != null )
            System.out.println( "Left: " + node.getLeft() );
        if( node.getRight() != null )
            System.out.println( "Right: " + node.getRight() );
        if( node.getLeft() != null )
            print( node.getLeft() );
        if( node.getRight() != null )
            print( node.getRight() );
    }
}
