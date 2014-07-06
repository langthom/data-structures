/**
 * Representation of a Node of a list.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class Node<T extends Comparable<T>>{

    T value;
    Node<T> next;
    Node<T> prev;

    /**
     * Constructor for a new Node.
     *
     * @param  T value
     *                  content of the Node.
     */
    public Node( T value ){
        this.value = value;
        next = null;
        prev = null;
    }

    /**
     * String representation of a single Node.
     *
     * @return The String representation of a single Node.
     */
    public String toString(){
        return ( "{" + value + "}" );
    }
}
