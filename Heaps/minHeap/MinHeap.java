/**
 * Implementation of a Min-Heap.
 * A Min-Heap is a specialization of a Heap that is used for
 * the implementation of many algorithms, especially on Graphs.
 * The difference to a normal Heap is that the root is always
 * the global minimum of all elements.
 * To achieve that a "bubble-up" will be performed if necessary.
 *
 * @author Thomas Lang
 * @version July 15, 2014
 * @see <a href="http://en.wikipedia.org/wiki/Min-max_heap">Min-max heap</a>
 */
public class MinHeap<T extends Comparable<T>> extends Heap<T>{

    /**
     * Constructor for a new Min-Heap, 
     * just calls the Heap-constructor.
     */
    public MinHeap(){
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
     * Performs the "bubble-Up" operation. So it checks, if
     * the parent Node is greater. If yes, the two will be
     * swapped and then this goes recursively through the
     * entire Heap.
     * So we can ensure, that the root of the Heap is always
     * the global minimum of all elements.
     *
     * @param  bNode  The Node that will be "bubble-up'ed".
     */
    private void bubbleUp( Node<T> bNode ){
        if( bNode.getParent() == null )
            return;
        if( bNode.getValue().compareTo( bNode.getParent().getValue() ) < 0 ){
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
