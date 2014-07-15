import java.util.NoSuchElementException;

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
    private static Object last; // pointer to last Node added

    /**
     * Constructor for a new Heap, initializes 
     * the <code>root</code> to <code>null</code>
     * and the <code>size</code> to 0.
     */
    public Heap(){
        root = null;
        size = 0;
        last = null;
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
     * Gets the size of the Heap.
     *
     * @return The size of the heap.
     */
    public int getSize(){
        return size;
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
            last = node;
        }else{
            insert( root, node );
            size++;
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
            last = newNode;
        }else if( node.getLeft() == null ){
            node.setLeft( newNode );
            newNode.setParent( node );
            last = newNode;
        }else if( node.getRight() == null ){
            node.setRight( newNode );
            newNode.setParent( node );
            last = newNode;
        }else if( isFullTree( node ) && isFullTree( node.getLeft() )){
            insert( node.getRight(), newNode );
        }else if( isFullTree( node ) ){
            insert( node.getLeft(), newNode );
        }
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
     * Gets the last Node inserted into the Heap.
     *
     * @return The last Node inserted into the Heap.
     * @throws NoSuchElementException
     */
    protected Node<T> getLastNode() throws NoSuchElementException{
        if( isEmpty() )
            throw new NoSuchElementException( "Error! No elements in Heap." );
        else
            return (Node<T>)last;
    }

    
    /**
     * Printing a Heap to stdout.
     */
    public void print(){
        if( isEmpty() )
            System.out.println( "(empty tree)" );
        System.out.println( "Root: " + root );
        printTreeRec( root );
    }

    /**
     * Recursive helper function for getting an appropiate
     * String representation of a Heap.
     *
     * @param  node  Current node.
     */
    private void printTreeRec( Node<T> node ){
        if( node.getLeft() != null )
            System.out.println( "Left: " + node.getLeft() + " "  );
        if( node.getRight() != null )
            System.out.println( "Right: " + node.getRight() );
        if( node.getLeft() != null )
            printTreeRec( node.getLeft() );
        if( node.getRight() != null )
            printTreeRec( node.getRight() );
    }
}
