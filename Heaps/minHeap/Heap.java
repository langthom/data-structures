/**
 * Abstract class for a Heap. 
 * A Heap is a binary tree following the rule, that a
 * new element is inserted at the first free space in
 * the tree and that each node is bigger than its parent
 * Node. If the second rule is violated after inserting
 * a new element, a "bubble-up" will be performed until
 * everything is okay.
 *
 * @author Thomas Lang
 * @version July 15, 2014
 * @see <a href="http://en.wikipedia.org/wiki/Binary_heap">Binary heap</a>
 */
public abstract class Heap<T extends Comparable<T>>{

    protected Node<T> root;
    protected int size;

    /**
     * Constructor for a new Heap, initializes 
     * the <code>root</code> to <code>null</code>
     * and the <code>size</code> to 0.
     */
    public Heap(){
        root = null;
        size = 0;
    }

    /**
     * Checks, whether the heap is emtpy or not.
     *
     * @return <code>true</code> if the heap is empty, <code>false</code> otherwise
     */
    public boolean isEmpty(){
        return ( root == null );
    }

    /**
     * Inserts a new Node to the heap.
     *
     * @param node  The node to insert
     */
    public void insert( Node<T> node ){
        if( isEmpty() ){
            root = node;
            size++;
        }else{
            insert( root, node );
        }
    }

    /**
     * Inserts a new Node recursively into the Heap.
     * As this is just the abstract Heap, the node will
     * only be inserted, but not moved.
     *
     * @param  node     The current node.
     * @param  newNode  The new Node to insert.
     */
    private void insert( Node<T> node, Node<T> newNode ){
        if( node == null ){
            node = newNode;
        }else if( node.getLeft() == null ){
            node.setLeft( newNode );
            newNode.setParent( node );
        }else if( node.getRight() == null ){
            node.setRight( newNode );
            newNode.setParent( node );
        }else if( isFullTree( node )){
            insert( node.getLeft(), newNode );
        }else if( isFullTree( node ) && isFullTree( node.getLeft() )){
            insert( node.getRight(), newNode );
        }
        size++;
    }

    /**
     * Helper function for checking if a node has both
     * a left and a right child.
     *
     * @param  node  The node to check.
     * @return <code>true</code> if the node has both children, <code>false</code> otherwise
     */
    private boolean isFullTree( Node<T> node ){
        if(( node != null ) && ( node.getLeft() != null ) && ( node.getRight() != null ))
                return true;
        return false;
    }

    /**
     * String representation of a Heap.
     *
     * @return A String representation of a Heap.
     */
    public String toString(){
        if( isEmpty() )
            return "(empty tree)";
        StringBuilder out = new StringBuilder();
        out.append( "Root: " + root + "\n" );
        out.append( treeString( root ) );
        return out.toString();
    }

    /**
     * Recursive helper function for getting an appropiate
     * String representation of a Heap.
     *
     * @param  node  Current node.
     * @return A String representation of the Heap.
     */
    private String treeString( Node<T> node ){
        StringBuilder out = new StringBuilder();
        if( node.getLeft() != null )
            out.append( "Left: " + node.getLeft() + " "  );
        if( node.getRight() != null )
            out.append( "Right: " + node.getRight() );
        out.append( "\n" );
        if( node.getLeft() != null )
            treeString( node.getLeft() );
        if( node.getRight() != null )
            treeString( node.getRight() );
        return out.toString();
    }
}
