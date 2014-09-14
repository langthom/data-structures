import java.util.List;
import java.util.LinkedList;

/**
 * Representation of a Graph.
 * A Graph contains edges between
 * nodes with a specific length.
 *
 * @author Thomas Lang
 * @version 2014/08/24
 */
public class Graph<T extends Comparable<T>> {

    private List<Edge<T>> edges;
    private int size;

    /**
     * Creates a new but empty graph.
     */
    public Graph(){
        edges = new LinkedList<Edge<T>>();
    	size  = 0;
    }

    /**
     * Returns the number of edges.
     *
     * @return The number of edges.
     */
    public int size(){ 
	    return size; 
    }

    /**
     * Adds a new Edge ranging from <code>src</code>
     * to <code>dest</code> with <code>length</code>
     * units in length.
     *
     * @param src    The source Node of the edge.
     * @param dest   The destination Node of the edge.
     * @param length The length of the edge.
     */
    public void add( T src, T dest, int length ){
        Node<T> src_  = getNode( src );
        Node<T> dest_ = getNode( dest );
	    
        if( src_ == null ){
            src_ = new Node<T>( src );
        }
        if( dest_ == null ){
            dest_ = new Node<T>( dest );
        }
        edges.add( new Edge( src_, dest_, length ) );
	    size++;
    }

    /**
     * Searches for a Node containing <code>value</code>
     * in the list of edges. If the Node is not in the
     * list (not in the graph), it returns NULL.
     *
     * @param  value The value of the Node to search.
     * @return The Node if found, null otherwise.
     */
    public Node<T> getNode( T value ){
        for( Edge<T> e : edges ){
            if( e.source().value() == value ){
                return e.source();
            }else if( e.destination().value() == value ){
                return e.destination();
            }
        }
        return null;
    }

    /**
     * Removes the Node <code>node</code> from the 
     * graph and so also all edges to/from it.
     *
     * @param node The node to remove.
     */
    public void remove( T node_ ){
        Node<T> node = getNode( node_ );
        if( node_ == null ){
            System.err.println( "Node not found!" );
        }else{
	        for( Edge<T> e : edges ){
	            if( e.source().equals( node ) ||
		            e.destination().equals( node ) ){
		             edges.remove( e );
		             size--;
	            }
	        }
        }
    }

    /**
     * Returns an appropriate String representation 
     * of the node, which simply displays all edges.
     *
     * @return A String representation of the graph.
     */
    public String toString(){
	    StringBuilder out = new StringBuilder();
	    for( Edge<T> edge : edges ){
	        out.append( edge );
	        out.append( "\n" );
	    }
	    return out.toString();
    }
}
