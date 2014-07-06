/**
 * Implementation of a typical Queue using 
 * a double linked list.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class Queue<T extends Comparable<T>>{

    DoubleLinkedList<T> q;

    /**
     * Constructor, initializes the list.
     */
    public Queue(){ q = new DoubleLinkedList<T>(); }

    /**
     * Checks whether the Queue is empty or not.
     *
     * @return true if it is empty, false otherwise
     */
    public boolean isEmpty(){ return q.isEmpty(); }

    /**
     * Enqueues a new Node containing <code>element</code> 
     * to the Queue.
     *
     * @param  T element
     *                   Content of the Node to insert
     */
    public void enqueue( T element ){
        q.addFront( element );
    }

    /**
     * Dequeues (removing the top element of the Queue)
     *
     * @return The popped element.
     */
    public Node<T> dequeue(){
        return q.pop();
    }

    /**
     * String representation of a Queue.
     * 
     * @return String representation of a Queue.
     */
    public String toString(){
        return q.toString();
    }
}
