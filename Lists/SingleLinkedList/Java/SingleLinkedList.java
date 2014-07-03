/**
 * Implementation of a generic single linked list.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class SingleLinkedList<T extends Comparable<T>>{

    /**
     * Representation of a Node of a list.
     *
     * @author Thomas Lang, 2014
     * @version 0.1
     */
    private class Node<T extends Comparable<T>>{
        T value; 
        Node<T> next;

        /**
         * Constructor for a new Node.
         *
         * @param  T value
         *                  content of the node.
         */
        private Node( T value ){
            this.value = value;
        }

        /**
         * String representation of a single Node.
         *
         * @return the String representation of a single Node.
         */
        public String toString(){ 
            return ( "{" + value + "}" );
        }
    }

    Node<T> front, rear;
    int size = 0;

    /**
     * Constructor for a new list, that sets the front,
     * and the rear pointer to NULL and the size to 0.
     */
    public SingleLinkedList(){
        front = null; 
        rear = null;
        size = 0;
    }

    /**
     * Checks whether a list is empty or not.
     *
     * @return  <code>true</code>, if the list is empty, <code>false</code> otherwise.
     */
    public boolean isEmpty(){ return ( front == null ); }

    /**
     * Adds a new Node containing <code>value</code> 
     * to the rear of the list.
     *
     * @param  T value
     *                  The value to insert.
     */
    public void add( T value ){
        Node<T> newNode = new Node( value );
        if( isEmpty() ){
            front = newNode;
            rear = newNode;
            size++;
        }
        else{
            rear.next = newNode;
            rear = newNode;
            size++;
        }
    }

    /**
     * Inserts a new Node containing <code>i</code> directly after
     * the first occurence of the Node in the list containing the
     * value <code>j</code>.
     *
     * @param  T i
     *              Value of new Node 
     * @param  T j
     *              Value of the Node, after which the new Node 
     *              has to be inserted
     */
    public void insertAfter( T i, T j ){
        if( isEmpty() )
            return;
        else{
            boolean found = false;
            Node<T> nodeJ = null;
            Node<T> f = front;
            while(( f.next != null ) && !found ){ 
                if( f.value.compareTo( j ) == 0 ){
                    nodeJ = f;
                    found = true;
                }
                f = f.next;
            }
            if( nodeJ == rear )
                add( i );
            else{
                Node<T> tmp = nodeJ.next;
                nodeJ.next = new Node<T>( i );
                nodeJ.next.next = tmp;
            }
            size++;
        }
    }

    /**
     * String representation of a list.
     *
     * @return String representation of a list.
     */
    public String toString(){
        StringBuilder out = null;

        if( isEmpty() )
            return "(empty list)";
        else{
            out = new StringBuilder();
            out.append( "[ " );
            Node<T> f = front;
            while( size >= 0 && f.next != null ){
                f = f.next;
                out.append( f.value + " " );
            }
            out.append( "]" );
        }
        return out.toString();
    }
}
