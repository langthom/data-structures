/**
 * String representation of a Node.
 * This module represents a single Node element
 * of a Heap. Such a node has a value and references
 * to its left and right child and to its parent Node.
 * Furthermore it includes a constructor and getters
 * and setters for all that things and a String representation.
 *
 * @author Thomas Lang
 * @version July 15, 2014
 */
public class Node<T extends Comparable<T>>{

    private T value;
    private Node<T> left;
    private Node<T> right;
    private Node<T> parent;

    /**
     * Constructor for a new Node element.
     * It saves the passed value and 
     * initializes all references to null.
     *
     * @param  value   the value to store.
     */
    public Node( T value ){
        this.value = value;
        left       = null;
        right      = null;
        parent     = null;
    }

    /**
     * Getter for the value.
     *
     * @return The value saved in a Node.
     */
    public T getValue(){        return value;  }

    /**
     * Getter for the left child.
     *
     * @return A pointer to the left child of the calling Node.
     */
    public Node<T> getLeft(){   return left;   }

    /**
     * Getter for the right child.
     *
     * @return A pointer to the right child of the calling Node.
     */
    public Node<T> getRight(){  return right;  }

    /**
     * Getter for the parent child.
     *
     * @return A pointer to the parent Node of the calling Node.
     */
    public Node<T> getParent(){ return parent; }

    /**
     * Setter for the Value
     *
     * @param  value   New value to store.
     */
    public void setValue( T value ){       this.value  = value; }

    /**
     * Setter for the left child.
     *
     * @param  node    New <code>left</code> reference.
     */
    public void setLeft(   Node<T> node ){ this.left   = node;  }

    /**
     * Setter for the right child.
     *
     * @param  node    New <code>right</code> reference.
     */
    public void setRight(  Node<T> node ){ this.right  = node;  }

    /**
     * Setter for the parent Node.
     *
     * @param  node    New <code>parent</code> reference.
     */
    public void setParent( Node<T> node ){ this.parent = node;  }

    /**
     * String representation of a single Node.
     *
     * @return String representation of the calling Node.
     */
    public String toString(){  return ( "{" + value + "}" ); }
}
