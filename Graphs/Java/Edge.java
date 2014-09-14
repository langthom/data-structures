/**
 * Representation of a single edge.
 * An edge ranges from one Node to
 * an other and has a defined length.
 *
 * @author Thomas Lang
 * @version 2014/08/24
 */
public class Edge<T extends Comparable<T>> {
    
    private Node<T> source;
    private Node<T> destination;
    private int length;

    /**
     * Creates a new Edge spanning from 
     * <code>src</code> to <code>dest</code>
     * with a length of <code>len</code>.
     *
     * @param src  The source Node of the edge.
     * @param dest The destination Node of the edge.
     * @param len  The length of the edge.
     */
    public Edge( Node<T> src, Node<T> dest, int len ){
    	if( len < 0 ){
	        System.err.println( 
			       "Error! Length of edge must be" +
			       " greather or equals 0." );
        }
    	source      = src;
	    destination = dest;
	    length      = len;
    }

    /**
     * Returns the source of the edge.
     *
     * @return The sourc of the edge.
     */
    public Node<T> source(){
        return source;
    }

    /**
     * Returns the destination of the edge.
     *
     * @return The destination of the edge.
     */
    public Node<T> destination(){
        return destination;
    }

    /**
     * Returns the length of the edge.
     *
     * @return The length of the edge.
     */
    public int length(){
	    return length;
    }

    /**
     * Returns a String representation
     * of the edge.
     *
     * @return A String representation.
     */
    public String toString(){
	    String str = "(" + source + 
	        " -> " + destination + ") " +
	        length;
	    return str;
    }
}
