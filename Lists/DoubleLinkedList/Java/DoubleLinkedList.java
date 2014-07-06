/**
 * Implementation of a generic double linked list.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class DoubleLinkedList<T extends Comparable<T>>{

    /**
     * Representation of a Node of a list.
     *
     * @author Thomas Lang, 2014
     * @version 0.1
     */
    private class Node<T extends Comparable<T>>{
        T value; 
        Node<T> next;
        Node<T> prev;

        /**
         * Constructor for a new Node.
         *
         * @param  T value
         *                  content of the node.
         */
        private Node( T value ){
            this.value = value;
            next = null;
            prev = null;
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
    public DoubleLinkedList(){
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
            newNode.prev = rear;
            rear = newNode;
            size++;
        }
    }

    /**
     * Adds a new Node containing <code>value</code>
     * to the beginning of the list.
     *
     * @param  T value
     *                  The value to insert.
     */
    public void addFront( T value ){
        if( isEmpty() )
            add( value );
        else{
            Node<T> newNode = new Node<T>( value );
            front.prev = newNode;
            newNode.next = front;
            front = newNode;
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
                Node<T> newNode = new Node<T>( i );
                nodeJ.next = newNode;
                newNode.prev = nodeJ;
                nodeJ.next.next = tmp;
            }
            size++;
        }
    }

    /**
     * Performs the Stack operation "pop", which
     * returns the last element of the list after
     * removing it.
     *
     * @return The last element of the list.
     */
    public Node<T> pop(){
        Node<T> last = rear;
        rear = rear.prev;
        rear.next = null;
        size--;
        return last;
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
            while( f != null ){
                out.append( f.value + " " );
                f = f.next;
            }
            out.append( "]" );
        }
        return out.toString();
    }
}
