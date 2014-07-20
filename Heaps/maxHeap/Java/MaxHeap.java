import java.util.NoSuchElementException;

/**
 * Implementation of a Max-Heap.
 * A Max-Heap is a specialization of a Heap that is used for
 * the implementation of many algorithms, especially on Graphs.
 * The difference to a normal Heap is that the root is always
 * the global maximum of all elements.
 * To achieve that a "bubble-up" will be performed if necessary.
 *
 * @author Thomas Lang
 * @version July 15, 2014
 * @see <a href="http://en.wikipedia.org/wiki/Min-max_heap">Min-max heap</a>
 */
public class MaxHeap<T extends Comparable<T>> extends Heap<T>{

    /**
     * Constructor for a new Max-Heap, 
     * just calls the Heap-constructor.
     */
    public MaxHeap(){
        super();
    }

    /**
     * Inserts a new Node containing <code>value</code> to
     * the Heap. The newly created Node will be inserted via
     * the <code>insert</code>-method in the abstract class 
     * and then a bubble-up is performed.
     *
     * @param  value  The value of the new Node to insert.
     */
    public void insert( T value ){
        Node<T> node = new Node<T>( value );
        super.insert( node );
        bubbleUp( node );
    }

    /**
     * Performs the "delete max" operation on the Heap.
     * This means that the root (that is always the maximum
     * of all elements) will be swapped with the last node
     * inserted. After that, that node will be deleted and
     * the new root will be "bubble-down'ed" until 
     * everything is okay again.
     *
     * @throws NoSuchElementException
     */
    public void deleteMax() throws NoSuchElementException{
        Node<T> last = getLastNode();
        root.setValue( last.getValue() );
        // deleting "last" node
        if( last.getParent().getLeft() == last ){
            last.getParent().setLeft( null );
            last.setParent( null );
        }else{
            last.getParent().setRight( null );
            last.setParent( null );
        }
        size--;

        bubbleDown( root );
    }

 
    /**
     * Does the "bubble down".
     * So it swaps a passed <code>node</code> with either
     * its left or right child until the ordering in the
     * Heap is restored.
     *
     * @param  node  Current node.
     */
    private void bubbleDown( Node<T> node ){
        // if node has no children, we cannot make bubbleDown
        if(( node.getLeft() == null ) && ( node.getRight() == null ))
            return;

        // if node has left and right children
        if(( node.getLeft() != null ) && ( node.getRight() != null )){
            if(( node.getValue().compareTo( node.getLeft().getValue() ) < 0 ) &&
               ( node.getLeft().getValue().compareTo( node.getRight().getValue() ) > 0 ))
            {
                swapNodes( node, node.getLeft() );
                bubbleDown( node.getLeft() );
            }else if( node.getValue().compareTo( node.getRight().getValue() ) < 0 ){
                swapNodes( node, node.getRight() );
                bubbleDown( node.getRight() );
            }
        }

        // if left child only
        if(( node.getLeft() != null ) && ( node.getValue().compareTo( node.getLeft().getValue() ) < 0 )){
                swapNodes( node, node.getLeft() );
                bubbleDown( node.getLeft() );
        }

        // if right child only
        if(( node.getRight() != null ) && ( node.getValue().compareTo( node.getRight().getValue() ) < 0 )){
                swapNodes( node, node.getRight() );
                bubbleDown( node.getRight() );
            }
    }


    /**
     * Performs the "bubble-Up" operation. So it checks, if
     * the parent Node is smaller. If yes, the two will be
     * swapped and then this goes recursively through the
     * entire Heap.
     * So we can ensure, that the root of the Heap is always
     * the global maximum of all elements.
     *
     * @param  bNode  The Node that will be "bubble-up'ed".
     */
    private void bubbleUp( Node<T> bNode ){
        if( bNode.getParent() == null )
            return;
        if( bNode.getValue().compareTo( bNode.getParent().getValue() ) > 0 ){
            swapNodes( bNode, bNode.getParent() );
            bubbleUp( bNode.getParent() );
        }
    }

    /**
     * Helper method for swapping two Nodes.
     *
     * @param  child   The first Node to swap.
     * @param  parent  The second Node to swap.
     */
    private void swapNodes( Node<T> child, Node<T> parent ){
        T childValue = child.getValue();
        child.setValue( parent.getValue() );
        parent.setValue( childValue );
    }
}
