import java.util.Queue;
import java.util.List;
import java.util.LinkedList;
import java.util.stream.*;

/**
 * Implementation of a generic binary search tree in Java.
 *
 * @author  Thomas Lang
 * @version 1.0, 04/02/2015
 */
public class BinarySearchTree<T extends Comparable<T>>{

    /**
     * Class representing a single Node of the tree.
     *
     * @author  Thomas Lang
     * @version 1.0, 04/02/2015
     */
    private class Node<T extends Comparable<T>>{
        
        /** Value of the node. */
        private T value;

        /** Reference to the left child. */
        private Node<T> left;
        
        /** Reference to the right child. */
        private Node<T> right;
        
        /** Reference to the parent node. */
        private Node<T> parent;
 
        /**
         * Constructor, saves the passed generic value.
         *
         * @param  value
         *              The value to save
         */
        private Node(T value){
            this.value = value;
        }

        /**
         * Returns the value of the node.
         *
         * @return Returns the value of the node.
         */
        public T getValue() {
            return value;
        }

        /**
         * Gets a String representation of a node which is simply its value.
         *
         * @return A String representation of a node.
         */
        @Override
        public String toString(){
            return "" + value;
        }
    }

    /** The root of the entire tree. */
    Node<T> root;

    /** The size of the tree which represents the number of elements in it. */
    int size;

    /**
     * Constructor, initializes the tree to null.
     */
    public BinarySearchTree() { 
        root = null; 
    }

    /**
     * Adds a new node containing the value "value".
     *
     * @param  value   Value to insert.
     * @return {@code true} if the element is not inserted into the tree yet,
     *         {@code false} otherwise.
     */
    public boolean add(T value) {

        if (isEmpty()) {
            root = new Node<T>(value);
            ++size;
            return true;
        } else {
            return insertInTree(root, value);
        }
    }

    /**
     * Inserts a new node containing the value "value" to the binary search 
     * tree by using it's property that all elements with a smaller value than 
     * the actual node are left of this, the ones greater then the actual node
     * are right of it.
     *
     * @param  node   Actual node, for traversing
     * @param  value  Value to insert.
     * @return {@code true} if the element was not inserted into the tree yet,
     *         {@code false} otherwise.
     */
    private boolean insertInTree(Node<T> node, T value) {
        
        if (value.compareTo(node.value) < 0) {
            if (node.left != null) {
                return insertInTree(node.left, value);
            } else {
                node.left = new Node<T>(value);
                node.left.parent = node;
                ++size;
                return true;
            }
        } else if (value.compareTo(node.value) > 0) {
            if (node.right != null) {
                return insertInTree(node.right, value);
            } else {
                node.right = new Node<T>(value);
                node.right.parent = node;
                ++size;
                return true;
            }
        } else {
            return false; // no duplicates allowed
        }
    }

    /**
     * Returns a list containing all elements from the tree.
     *
     * @return A list containing all elements from the tree.
     */
    public List<T> getAll() {
        List<T> all = new LinkedList<T>();
        Queue<Node<T>> queue = new LinkedList<Node<T>>();
        all.add(root.getValue());
        queue.offer(root);

        while (!queue.isEmpty()) {
            final Node<T> currentNode = queue.poll();
            final Node<T> leftChild = currentNode.left;
            final Node<T> rightChild = currentNode.right;

            if (leftChild != null) {
                all.add(leftChild.getValue());
                queue.offer(leftChild);
            }

            if (rightChild != null) {
                all.add(rightChild.getValue());
                queue.offer(rightChild);
            }
        }

        return all;
    }

    /**
     * Clears the entire tree so that no elements are in it after this.
     */
    public void clear() {
        root = null;
    }

    /**
     * Deletes the node containing the value "element" from the tree.
     *
     * @param  element  Value of the node-to-delete.
     * @return {@code true} if the element was deleted successfully, 
     *         {@code false} otherwise.
     */
    public boolean delete(T element) {
        
        if (isEmpty()) {
            return false;
        }

        Node<T> nodeToDelete = findNode(element);

        if (nodeToDelete != null) {
            if (nodeToDelete.value.compareTo(root.value) == 0) {
                deleteRoot();
            } else if ((nodeToDelete.left != null) && (nodeToDelete.right != null)) {
                deleteInternalNode(nodeToDelete);
            } else { 
                deleteNode(nodeToDelete);
            }

            --size;
            return true;
        }else{
            return false; // element not found
        }
    }

    /**
     * Deletes the root element.
     */
    private void deleteRoot() {
        deleteInternalNode(root);
    }

    /**
     * Deletes an internal node by swapping the node-to-delete with the 
     * next-smaller node (which has either none or one child), and then 
     * deleting this node.
     *
     * @param  node  Node-to-delete.
     */
    private void deleteInternalNode(Node<T> node) {
        // deleting internal node by swapping with
        // maximum of left child-subtree
        assert node != null : "Parameter must not be null.";

        Node<T> maxOfMin  = findMaxOfMinNode(node);
        
        if (maxOfMin == null) {
            return;
        }

        node.value = maxOfMin.value;
        deleteNode(maxOfMin);
    }

    /**
     * Deletes a node that has either one or no child.
     *
     * @param  node  The node to delete.
     */
    private void deleteNode(Node<T> node) {

        if ((node.left == null) && (node.right == null)) {
            if (node.parent.right.value.compareTo(node.value) == 0) {
                node.parent.right = null;
                node.parent = null;
            } else {
                node.parent.left = null;
                node.parent = null;
            }
        } else if (node.left != null) {
            if (node.parent.right.value.compareTo(node.value) == 0) {
                node.parent.right = node.left;
                node.left.parent = node.parent;
                node.parent = null;
            } else {
                node.parent.left = node.left;
                node.left.parent = node.parent;
                node.parent = null;
            }
        } else {
            if (node.parent.right.value.compareTo(node.value) == 0){
                node.parent.left = node.right;
                node.right.parent = node.parent;
                node.parent = null;
            } else {
                node.parent.right = node.right;
                node.right.parent = node.parent;
                node.parent = null;
            }
        }
    }

    /**
     * Checks if the passed {@code element} is contained within this tree or
     * not.
     *
     * @param element
     *              The element to search for, which must not be null.
     * @return {@code true} if the element is contained within this tree, 
     *         {@code false} otherwise.
     */
    public boolean contains(T element) {
        return findNode(element) != null;
    }

    /**
     * Finds a node containing the value "element" in the tree.
     *
     * @param   element  Value of the Node-to-Find
     * @return  The node with the value "element", {@code null} otherwise.
     */
    public Node<T> findNode(T element) {
        
        if(isEmpty()) {
            return null;
        } else {
            return findNodeRec(root, element);
        }
    }

    /**
     * Finds the node containing the value "element" by traversing
     * through the tree.
     *
     * @param  node     Current node.
     * @param  element  Value of the node-to-find.
     * @return The node, if found, {@code null} otherwise.
     */
    private Node<T> findNodeRec(Node<T> node, T element){
       
        if (node == null) {
            return null;
        }

        if (node.value.equals(element)) {
            return node;
        } else if (node.value.compareTo(element) < 0) {
            return findNodeRec(node.left, element);
        } else {
            return findNodeRec(node.right, element);
        }
    }

    /**
     * Finds the next-smaller node in the tree.
     *
     * @param  node  Current node.
     * @return next-smaller node in the tree.
     */
    public Node<T> findMaxOfMinNode( Node<T> node ){
        Node<T> leftChild = null;
        
        if (node.left != null) {
            leftChild = node.left;

            while (leftChild.right != null) {
                leftChild = leftChild.right;
            }
        }

        return leftChild;
    }

    /**
     * Checks whether the tree is empty or not.
     *
     * @return true if tree is empty, false otherwise
     */
    public boolean isEmpty() { 
        return root == null; 
    }

    /**
     * Returns the number of {@code Nodes} currently stored in the tree.
     *
     * @return The size of the tree.
     */
    public int size() {
        return size;
    }

    /**
     * Returns a String representation of a binary search tree.<p>
     * This representation simply are the nodes as the tree is traversed
     * in breadth first search.
     *
     * @return A String representation of a binary search tree.
     */
    @Override
    public String toString() {

        if (isEmpty()) {
            return "(empty tree)";
        } else {
            StringBuilder repr = new StringBuilder();
            getAll().stream().forEach(elem -> 
                    { 
                        repr.append(elem); 
                        repr.append(", "); 
                    });
            final int len = repr.length();
            repr.delete(len - 2, len - 1);
            return repr.toString();
        }
    }
}
