/* Copyright (c) 2015, Thomas Lang. All rights reserved.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */

/**
 * Implementation of a red-black tree.<p>
 * A red-black tree is a binary search tree whose nodes can have colors (either
 * red or black). This tree is self-balancing and guarantees that all important
 * operations (like searching, inserting and removing values) run in 
 * {@code O(log n)} where {@code n} denotes the number of elements in the tree.
 * <p>
 * This rebalancing is done by guaranteeing the following rules:
 * <ul>
 *  <li>
 *    The root is always black.
 *  </li>
 *  <li>
 *    The leaf nodes (what are {@code null} pointers in reality) are black.
 *  </li>
 *  <li>
 *    If a node is red, both children are black.
 *  </li>
 *  <li>
 *    Every path from a node to its children contain the same amount of black 
 *    nodes.
 *  </li>
 * </ul>
 *
 * @param <T>
 *          Type parameter where any sub type of {@code Object} can be set in
 *          as long as this type implements the {@code Comparable} interface.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-08-05
 * @see Node
 */
public class RedBlackTree<T extends Comparable<T>> {
    
    /** The size of the tree. */
    private int size;

    /** The root node of the tree. */
    private Node<T> root;

    /**
     * Checks if the tree is empty or not.
     *
     * @return Returns either {@code true} if the tree is empty or
     *         {@code false} otherwise.
     */
    public boolean isEmpty() {
        return size == 0;
    }

    /**
     * Returns the number of elements stored in the tree.
     *
     * @return Returns the size of the tree.
     */
    public int size() {
        return size;
    }

    /**
     * Inserts the passed value into the tree.
     *
     * @param value
     *         The value to insert into the tree which must not be 
     *         {@code null}.
     * @return Returns either {@code true} if the insertion was successful or
     *         {@code false} otherwise.
     * @throws NullPointerException
     *         A {@code NullPointerException} will be thrown if the passed
     *         value was {@code null}.
     */
    public boolean insert(T value) {

        if (value == null) {
            throw new NullPointerException("Null value cannot be inserted.");
        }

        final Node<T> node = new Node<>(value);
        boolean success = true;

        if (isEmpty()) {
            node.color = Color.BLACK;
            root = node;
        } else {
            success = insert(root, node);
        }

        if (success) {
            ++size;
        }

        return success;
    }

    /**
     * Recursively traverses through the tree until the position for inserting
     * is found. Then the node is inserted. If necessary, the tree is also 
     * rebalanced starting from the new node.
     *
     * @param current
     *         The current traversal node which must not be {@code null}.
     * @param node
     *         The node to insert into the tree which must not be {@code null}.
     * @return Returns either {@code true} if the insertion was successful or
     *         {@code false} if the node already exists in the tree.
     * @see #rebalance(Node<T>)
     */
    private boolean insert(Node<T> current, Node<T> node) {
        assert current != null : "Null traversing node passed.";
        assert node != null : "Null node passed.";

        final T curValue = current.value;
        final T newValue = node.value;
        boolean success = true;

        if (newValue.compareTo(curValue) < 0) {
            if (current.left == null) {
                current.left = node;
                node.parent = current;
                rebalance(node);
                success = true;
            } else {
                success = insert(current.left, node);
            }
        } else if (newValue.compareTo(curValue) > 0) {
            if (current.right == null) {
                current.right = node;
                node.parent = current;
                rebalance(node);
                success = true;
            } else {
                success = insert(current.right, node);
            }
        } else {
            success = false;
        }

        return success;
    }

    /**
     * Rebalances the tree from {@code node} if necessary.
     *
     * @param node
     *          The node to start rebalancing from.
     */
    private void rebalance(Node<T> node) {
        assert node != null : "Null node passed.";

        Node<T> parent = node.parent;

        if (parent == null) {
            /*
             * Case 1: The new node has no parent. This means that the new node
             * is the root and the root always must be black.
             */
            node.color = Color.BLACK;
            return;
        }

        if (parent.color == Color.BLACK) {
            /*
             * Case 2: Per default every new node (including this one) are red.
             * When the color of the parent node is black, then the depth of
             * black nodes is still the same and we do not have to do anything.
             */
            return;
        }

        Node<T> grandParent = parent.parent;
        Node<T> uncle = (parent == grandParent.left) ? grandParent.right 
                                                     : grandParent.left;

        if ((uncle != null) && (uncle.color == Color.RED)) {
            /*
             * Case 3: Both the uncle and the parent nodes are red. Then we 
             * restore the tree by changing the below colors what makes the
             * tree be okay locally. But now, the grand parent will be
             * problematic, so we rebalance it.
             */
            parent.color = Color.BLACK;
            uncle.color = Color.BLACK;
            grandParent.color = Color.RED;
            rebalance(grandParent);
        } else {

            /*
             * Case 4: The parent node and the node itself are red and the
             * path from the grand parent to the node forms a zig-zag line.
             * Then we perform a rotation and swap positions what will result
             * in a constellation useable for the fifth case.
             * The exact rotation depends on if the node was a left or a right
             * child.
             */
            if ((node == parent.right) && (parent == grandParent.left)) {
                rotateLeft(parent);
                node = node.left;
            } else if ((node == parent.left) 
                    && (parent == grandParent.right)) {
                rotateRight(parent);
                node = node.right;
            }

            /*
             * Case 5: From this position we restore the tree by swapping
             * colors and rotations around the grand parent, depending on if
             * the node was a left or a right child.
             */
            parent = node.parent;
            grandParent = parent.parent;

            parent.color = Color.BLACK;
            grandParent.color = Color.RED;

            if ((node == parent.left) && (parent == grandParent.left)) {
                rotateRight(grandParent);
            } else {
                rotateLeft(grandParent);
            }
        }
    }

    /**
     * Performs a single left rotation around the passed {@code node}.
     *
     * @param node
     *          The node to rotate around which must not be {@code null}.
     */
    private void rotateLeft(Node<T> node) {
        assert node != null : "Null node passed.";

        Node<T> parent    = node.parent;
        Node<T> right     = node.right;
        Node<T> rightLeft = right.left;

        node.parent       = right;
        node.right        = rightLeft;

        right.parent      = parent;
        right.left        = node;

        if (rightLeft != null) {
            rightLeft.parent = node;
        }

        if (parent == null) {
            root = right;
        } else if (parent.left == node) {
            parent.left = right;
        } else {
            parent.right = right;
        }
    }

    /**
     * Performs a single right rotation around the passed {@code node}.
     *
     * @param node
     *          The node to rotate about which must not be {@code null}.
     */
    private void rotateRight(Node<T> node) {
        assert node != null : "Null node passed.";

        Node<T> parent    = node.parent;
        Node<T> left      = node.left;
        Node<T> leftRight = left.right;

        node.parent       = left;
        node.left         = leftRight;

        left.parent       = parent;
        left.right        = node;

        if (leftRight != null) {
            leftRight.parent = node;
        }

        if (parent == null) {
            root = left;
        } else if (parent.right == node) {
            parent.right = left;
        } else {
            parent.left = left;
        }
    }

    /**
     * Checks if the tree contains the passed {@code value}.
     *
     * @param value
     *         The value to search for.
     * @return Returns either {@code true} if the tree contains the passed
     *         {@code value} or {@code false} otherwise.
     * @see #get(T)
     */
    public boolean contains(T value) {
        
        if (value == null) {
            return false;
        }

        return get(value) != null;
    }

    /**
     * Gets the node containing the passed {@code value}.
     *
     * @param value
     *         The value to search for.
     * @return Returns either the node containing the passed {@code value} or
     *         {@code null} if the tree is empty or no node could be found.
     * @see #get(Node<T>, T)
     */
    private Node<T> get(T value) {
        if (isEmpty()) {
            return null;
        } else {
            return get(root, value);
        }
    }

    /**
     * Recursively gets the node containing the passed {@code value}.
     *
     * @param current
     *         The current traversal node, which must not be {@code null}.
     * @param value
     *         The value to search for.
     * @return Returns either the node containing the passed {@code value} or
     *         {@code null} if no such node could be found.
     */
    private Node<T> get(Node<T> current, T value) {
        assert current != null : "Null traversing node.";

        final T curValue = current.value;

        if (value.compareTo(curValue) == 0) {
            return current;
        } else if ((value.compareTo(curValue) < 0) && (current.left != null)) {
            return get(current.left, value);
        } else if ((value.compareTo(curValue) > 0) 
                && (current.right != null)) {
            return get(current.right, value);
        } else {
            return null;
        }
    }

    /**
     * Removes the passed {@code value} from this tree.<p>
     * If necessary, the tree will be rebalanced after the removal. If the 
     * passed {@code value} does not exist in the tree, this function does
     * nothing and returns {@code false}.
     *
     * @param value
     * @return Returns {@code true} if the removal was successful or 
     *         {@code false} otherwise.
     * @see #deleteInternalNode(Node<T>)
     * @see #deleteNode(Node<T>)
     */
    public boolean remove(T value) {

        if (value == null) {
            throw new NullPointerException("Null node cannot be deleted.");
        }

        final Node<T> node = get(value);

        if (node == null) {
            return false;
        }

        final Node<T> parent = node.parent;

        if (value.compareTo(root.value) == 0) {
            if ((root.left == null) && (root.right == null)) {
                root = null;
                return true;
            } else if ((root.left != null) && (root.right == null)) {
                Color c = root.color;
                root = root.left;
                root.color = c;
            } else if ((root.left == null) && (root.right != null)) {
                Color c = root.color;
                root = root.right;
                root.color = c;
            } else deleteInternalNode(root);
        } else 
            if ((node.left != null) && (node.right != null)) {
            deleteInternalNode(node);
        } else {
            deleteNode(node);
        }

        --size;
        return true;
    }

    /**
     * Deletes a node from the tree that has both two children.
     *
     * @param node
     *          The node to delete which must not be {@code null}.
     * @see #deleteNode(Node<T>)
     */
    private void deleteInternalNode(Node<T> node) {
        assert node != null : "Null node passed.";

        /*
         * The node is deleted by exchanging its value with the largest value
         * from the left sub tree and finally deleting the maximum node of the
         * left sub tree.
         */
        Node<T> maxOfMin = node.left;

        while (maxOfMin.right != null) {
            maxOfMin = maxOfMin.right;
        }

        node.value = maxOfMin.value;
        deleteNode(maxOfMin);
    }

    /**
     * Deletes a node from the tree that does not have two real children.
     * The tree will be rebalanced if necessary.
     *
     * @param node
     *          The node to delete which must not be {@code null}.
     * @see #rebalanceAfterDeletion(Node<T>)
     */
    private void deleteNode(Node<T> node) {
        assert node != null : "Null node passed.";

        Node<T> parent = node.parent;
        Node<T> left   = node.left;
        Node<T> right  = node.right;
        Node<T> pright = parent.right;
        boolean rightChild 
               = (pright != null) && (pright.value.compareTo(node.value) == 0);
        
        /*
         * Please note that we need not to check if 'parent' is null here,
         * because this can only happen if 'node' is the root, but this special
         * case is already recognized in the methode 'remove'.
         */

        if ((node.left == null) && (node.right == null)) {

            if (node.color == Color.BLACK) {
                rebalanceAfterDeletion(node);
            }

            if (rightChild) {
                parent.right = null;
            } else {
                parent.left  = null;
            }

        } else if (node.left != null) {

            if (rightChild) {
                parent.right = left;
            } else {
                parent.left = left;
            }

            left.parent = parent;
        } else {

            if (rightChild) {
                parent.right = right;
                parent.right.color = node.color;
            } else {
                parent.left = right;
                parent.left.color = node.color;
            }

            right.parent = parent;
        }

        node.parent = null;
        node = null;
    }

    /**
     * Rebalances the tree after a deletion.
     *
     * @param node
     *          A child of the deleted node which must not be {@code null}.
     */
    private void rebalanceAfterDeletion(Node<T> node) {
        assert node != null : "Null node passed.";

        final Node<T> parent = node.parent;

        if (parent == null) {
            /* Case 1: Problematic node is root, no rotations to made. */
            return;
        }

        final Node<T> sibling 
            = (node == parent.left) ? parent.right : parent.left;

        if (sibling.color == Color.RED) {
            /*
             * Case 2: The sibling of the node is red.
             * Then invert the colors of the parent and the sibling node
             * following by performing a left / right rotation around the
             * parent node depending on if the node was a left or a right
             * child.
             */
            parent.color = Color.RED;
            sibling.color = Color.BLACK;

            if (node == parent.left) {
                rotateLeft(parent);
            } else {
                rotateRight(parent);
            }
        }

        final Color pColor  = parent.color;
        final Color sColor  = sibling.color;
        final Color slColor 
            = sibling.left == null ? Color.BLACK : sibling.left.color;
        final Color srColor 
            = sibling.right == null ? Color.BLACK : sibling.right.color;

        if ((pColor == Color.BLACK) && (sColor == Color.BLACK) 
                && (slColor == Color.BLACK) && (srColor == Color.BLACK)) {
            /*
             * Case 3: The parent, the sibling and both children of the sibling
             * are black. Then the sibling has the wrong color, so change it to
             * red. This may have corrupted any integraty conditions of the
             * parent node, so we have to rebalance the parent node.
             */
            sibling.color = Color.RED;
            rebalanceAfterDeletion(parent);
        } else if ((pColor == Color.RED) && (sColor == Color.BLACK)
                && (slColor == Color.BLACK) && (srColor == Color.BLACK)) {
            /*
             * Case 4: The sibling and its both children are black but the 
             * parent is red. Then we can rebalance the tree by simply 
             * inverting the colors of the sibling and parent node.
             */
            sibling.color = Color.RED;
            parent.color = Color.BLACK;
        } else {
            /*
             * Case 5:
             * (a): Node is the left child and the sibling and the sibling's
             *      right child are black but the siblings left child is red.
             *      Then we change the colors of the sibling and it's left 
             *      child and perform a right rotation around the sibling.
             *      Then all paths have the same number of right nodes.
             *      After this, we immediately go to case 6.
             * (b): The same thing as in (a) but the other way (right child).
             */
            if ((node == parent.left) && (sColor == Color.BLACK)
                    && (slColor == Color.RED) && (srColor == Color.BLACK)) {
                sibling.color = Color.RED;
                sibling.left.color = Color.BLACK;
                rotateRight(sibling);
            } else if ((node == parent.right) && (sColor == Color.BLACK)
                    && (slColor == Color.BLACK) && (srColor == Color.RED)) {
                sibling.color = Color.RED;
                sibling.right.color = Color.BLACK;
                rotateLeft(sibling);
            }

            /*
             * Case 6: The sibling is black, the right child of the sibling is
             * red and the node is the left child of it's parent.
             * Then we resolve this illegal state by changing the colors as
             * below. After this, we have to correct the now invalid paths
             * by rotating, depending on if the node was a left or a right 
             * child.
             */
            sibling.color = parent.color;
            parent.color = Color.BLACK;

            if (node == parent.left) {
                sibling.right.color = Color.BLACK;
                rotateLeft(parent);
            } else {
                sibling.left.color = Color.BLACK;
                rotateRight(parent);
            }
        }
    }

    /**
     * Clears the tree.
     */
    public void clear() {
        root = null;
        size = 0;
    }

    /**
     * Returns a string representation of the entire tree.<p>
     * This representation is either {@code (empty tree)} if the tree is empty
     * or the representation of the root node.
     *
     * @return Returns a string representation of the entire tree.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        if (isEmpty()) {
            return "(empty tree)";
        } else {
            return root.toString();
        }
    }

    /**
     * Implementation of a single node of a Red-black tree.<p>
     * This is basically a simple node of any binary search tree but with the
     * additional {@code color} attribute of type {@code Color}, what can be
     * either {@code Color.RED} or {@code Color.BLACK}.
     *
     * @param <T>
     *          Type parameter where any subclass of {@code Object} can be set
     *          in, as long as this type implements the {@code Comparable}
     *          interface.
     *
     * @author Thomas Lang
     * @version 2015-08-05
     * @see Color
     */
    static class Node<T extends Comparable<T>> {

        /** Color of this node. */
        private Color color;

        /** Value encapsulated in this node. */
        private T value;

        /** Pointer to this node's left child. */
        private Node<T> left;

        /** Pointer to this node's right child. */
        private Node<T> right;

        /** Pointer to this node's parent node. */
        private Node<T> parent;

        /**
         * Creates a new red node encapsulating the passed {@code value}.
         *
         * @param value
         *          The value encapsulated in this node.
         */
        Node(T value) {
            color = Color.RED;
            this.value = value;
        }

        /**
         * Returns a string representation of a node including all of its
         * children.
         *
         * @return Returns a string representation of a node.
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder repr = new StringBuilder();

            if (left != null) {
                repr.append("(");
                repr.append(left.toString());
                repr.append(")");
            }

            repr.append("[");
            repr.append(color);
            repr.append(": ");
            repr.append(value);
            repr.append("]");

            if (right != null) {
                repr.append("(");
                repr.append(right.toString());
                repr.append(")");
            }

            return repr.toString();
        }
    }

    /** Enum representing the two possible colors of a node. */
    static enum Color { RED, BLACK }

    /**
     * Main (testing) method.
     *
     * @param args
     *          Command line arguments (not used here).
     */
    public static void main(String[] args) {
        System.out.print("Creating a new Red-black tree ... ");
        RedBlackTree<Integer> test = new RedBlackTree<>();
        System.out.println("done.");
        System.out.print("Inserting some values ... ");
        test.insert(7);
        test.insert(8);
        test.insert(6);
        test.insert(12);
        test.insert(0);
        test.insert(9);
        test.insert(10);
        test.insert(-7);
        test.insert(999);
        System.out.println("done.");
        System.out.println("Tree now: " + test);

        System.out.print("Deleting a few values ... ");
        test.remove(12);
        test.remove(0);
        test.remove(9);
        test.remove(7);
        test.remove(8);
        test.remove(10);
        test.remove(-7);
        test.remove(6);
        System.out.println("done.");
        System.out.println("Tree now: " + test);

        System.out.print("Clearing tree now ... ");
        test.clear();
        System.out.println("done.");
        System.out.println("Tree now: " + test);
    }
}
