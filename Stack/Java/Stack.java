import java.util.NoSuchElementException;

/**
 * Implementation of a classical generic Stack.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class Stack<T extends Comparable<T>>{

    /**
     * Representation of a single Node element.
     *
     * @author Thomas Lang, 2014
     * @version 0.1
     */
    private class Node<T extends Comparable<T>>{
        T value;
        Node<T> next;

        /**
         * Constructor, creates a new Node
         * containing the content </code>value</code>.
         *
         * @param  T value
         *                  Value of newly created Node.
         */
        private Node( T value ){
            this.value = value;
            next = null;
        }

        /**
         * String representation of a single Node.
         *
         * @return String representation of a single Node.
         */
        public String toString(){
            return ( "{" + value + "}" );
        }
    }

    Node<T> sp;  // Note: sp stands for "stack pointer" (the pointer to the top element) 
    int size;

    /**
     * Constructor, initializes the stack pointer 
     * to NULL and the size to 0.
     */
    public Stack(){
        sp = null;
        size = 0;
    }

    /**
     * Checks whether a Stack is empty or not.
     *
     * @return <code>true</code> if the Stack is empty, <code>false</code> otherwise.
     */
    public boolean isEmpty(){ return ( sp == null ); }

    /**
     * Pushes a new Node containing the content <code>value</code> 
     * to the Stack.
     *
     * @param  T value
     *                  Value of the new Node.
     */
    public void push( T value ){
        Node<T> newNode = new Node<T>( value );
        if( isEmpty() ){
            sp = newNode;
            size++;
        }
        else{
            newNode.next = sp;
            sp = newNode;
            size++;
        }
    }

    /**
     * Deletes the top element and returns it.
     *
     * @return Top element of the Stack.
     * @throws NoSuchElementException
     */
    public Node<T> pop() throws NoSuchElementException {
        if( isEmpty() )
            throw new NoSuchElementException( "Error! Cannot pop from empty stack." );
        Node<T> top = sp;
        sp = sp.next;
        size--;
        return top;
    }

    /**
     * Returns the top element of the Stack
     * like pop, but NOT DELETING IT.
     *
     * @return Top element of the Stack.
     * @throws NoSuchElementException 
     */
    public Node<T> top() throws NoSuchElementException {
        if( isEmpty() )
            throw new NoSuchElementException( "Error! Cannot top from empty stack." );
        return sp;
    }

    /**
     * String representation of a Stack.
     *
     * @return String representation of a Stack.
     */
    public String toString(){
        if( isEmpty() )
            return "(empty stack)";
        StringBuilder out = new StringBuilder();
        Node<T> front = sp;
        out.append( "---> " + front.value + "\n" );
        while( front.next != null ){
            front = front.next;
            out.append( "     " + front.value + "\n" );
        }
        return out.toString();
    }
}
