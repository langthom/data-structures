/**
 * Representation of a single Node.
 * A Node holds a value.
 *
 * @author Thomas Lang
 * @version 2014/08/24
 */
public class Node<T extends Comparable<T>> {

    private T value;

    /**
     * Creates a new Node containing
     * the value <code>value</code>.
     *
     * @param value The value to save.
     */
    public Node( T value ){
    	this.value = value;
    }

    /**
     * Returns the value saved in the Node.
     *
     * @return The value saved in the Node.
     */
    public T value(){
        return value;
    }

    /**
     * Returns a String representation
     * of the Node, which simply returns
     * the String containing its value.
     *
     * @return A String representation.
     */
    public String toString(){
	    return "" + value;
    }
}
