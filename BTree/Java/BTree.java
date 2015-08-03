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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Iterator;
import java.util.ListIterator;

/**
 * Implementation of a B-tree.<p>
 * A B-tree is a tree data structure designed for holding a very large amount 
 * of values while still ensuring that access is performant. This is ensured
 * by having nodes that can hold much more than only a single value and a node
 * can have more than only two children.<p>
 * Consider the following example:
 * The access time for searching in a binary search tree containing {@code n}
 * nodes is {@code log_2(n)}, so a search in a tree containing one million
 * nodes it would take {@code log_2(1,000,000) &#8776; 20} comparisons. In a
 * B-tree with a degree of 512 would take {@code log_1025(1,000,000) &#8776; 2}
 * comparisons for traversing, about <strong>ten times less</strong>!
 * This is one reason while B-trees are often used in database management 
 * systems. Note that the degree is a constant number, so the times for 
 * searching in a node is negligible.<p>
 * All common operations (searching, insertion, deletion) run in 
 * {@code O(log n)} where {@code n} denotes the size of the tree, both in 
 * average and worst case.
 *
 * @author Thomas Lang
 * @version 1.1, 2015-08-03
 * @see <a href="https://en.wikipedia.org/wiki/B-tree">B-trees on Wikipedia</a>
 * @see #BTree()
 */
public class BTree<T extends Comparable<T>> {

    /** The degree of this tree. */
    private int degree;

    /** The size of the tree. */
    private int size;
    
    /** Pointer to the root node. */
    private Node<T> root;

    /**
     * Creates a new {@code BTree} with the passed degree.<p>
     * The degree of a B-tree is here defined as follows:
     * The result of {@code 2 * degree} is the maximum size of a single node
     * before splitting. So, a degree of {@code 1} means that a single node can
     * hold a maximum of 2 values, if a third value will be added, then this
     * node is splitted up into three nodes. As a consequence, a single node
     * can hold a maximum of {@code 2 * degree + 1} references to its children.
     *
     * @param degree
     *          The degree of this B-tree.
     * @see #insert(T)
     */
    public BTree(int degree) {
        this.degree = degree;
    }

    /**
     * Checks if this tree is empty or not.
     *
     * @return Returns either {@code true} if the tree has no elements, 
     *         {@code false} otherwise.
     */
    public boolean isEmpty() {
        return root == null;
    }

    /**
     * Returns the amount of values stored in this tree.
     *
     * @return Returns the size of the tree.
     */
    public int size() {
        return size;
    }

    /**
     * Inserts the passed {@code value} into this tree.<p>
     * If the tree is empty, then the value becomes a single node and the root
     * of the tree. If the tree is not empty, then the node is searched, where
     * the value should be inserted to.
     * When we got this node, we insert the value and rebalance the tree if
     * necessary.
     *
     * @param value
     *         The value to insert into the tree.
     * @return Returns {@code true} if the insertion proceeded successfully or
     *         {@code false} otherwise.
     * @throws IllegalStateException
     *         An {@code IllegalStateException} will be thrown if no node can
     *         be found to insert to.
     * @see #get(Node<T>, T)
     * @see #rebalance(Node<T>)
     */
    public boolean insert(T value) {

        if (isEmpty()) {
            Node<T> node = new Node<>();
            node.values.add(value);
            node.refs.add(null);
            node.refs.add(null);
            root = node;
            return true;
        }

        final Node<T> node = get(root, value);

        ListIterator<T> valIt = node.values.listIterator();
        ListIterator<Node<T>> refIt = node.refs.listIterator();
        refIt.next();

        while (valIt.hasNext()) {
            T itValue = valIt.next();

            if (value.compareTo(itValue) < 0) {
                valIt.previous();
                valIt.add(value);
                refIt.previous();
                refIt.add(null);
                break;
            } else if (value.compareTo(itValue) == 0) {
                /* No duplicates allowed. */
                return false;
            } else if ((valIt.nextIndex() == node.values.size())
                    && (value.compareTo(itValue) > 0)) {
                /*
                 * Here is the special case that the value is greater than 
                 * every other value in the node where the iterator would fail.
                 */
                node.values.add(value);
                node.refs.add(null);
                break;
            }
        }

        if (node.isFull()) {
            rebalance(node);
        }

        ++size;
        return true;
    }

    /**
     * Rebalances the tree starting at {@code node}.<p>
     * Such a rebalancing only happens, if the passed node is full.
     * If so, the passed node will be split up into two nodes. The first one 
     * contains all values and references to children with values 
     * <em>smaller</em> than the <strong>median</strong> of all values in the
     * node. The second one analogous contains all values and references 
     * <em>greater</em> than the median of all values. If the passed node was
     * the root, then the median will become the new root, else the median will
     * be inserted into the parent node and the rebalancing continues on the
     * parent node.
     *
     * @param node
     *         The node to start rebalancing at which must not be {@code null}.
     */
    private void rebalance(Node<T> node) {
        assert (node != null) : "Null node passed.";

        if (!node.isFull()) {
            /* If the node is not full, there is nothing to rebalance. */
            return;
        }

        /*
         * First, find the median of all values of the node and create the left
         * and right children.
         *
         * As the values in nodes are already sorted, it is easy to determine 
         * this things.
         */
        int medianIndex = (int) Math.ceil(node.values.size() / 2);
        T median = node.values.get(medianIndex);

        Node<T> leftChild = new Node<>();
        Node<T> rightChild = new Node<>();

        leftChild.values.addAll(node.values.subList(0, medianIndex));
        leftChild.refs.addAll(node.refs.subList(0, medianIndex + 1));
        rightChild.values.addAll(
                node.values.subList(medianIndex + 1, node.values.size()));
        rightChild.refs.addAll(
                node.refs.subList(medianIndex + 1, node.refs.size()));

        if (node == root) {
            /* If the node was the root, we'll create a new root. */
            Node<T> newRoot = new Node<>();
            newRoot.values.add(median);
            newRoot.refs.add(leftChild);
            newRoot.refs.add(rightChild);
            leftChild.parent = newRoot;
            rightChild.parent = newRoot;

            /* 
             * If we create a new node, it may be that the parent pointers of
             * the children contain old references, so we must refresh them.
             */
            leftChild.refs.stream().forEach(c -> {
                if (c != null) {
                    c.parent = leftChild;
                }
            });
            
            rightChild.refs.stream().forEach(c -> {
                if (c != null) {
                    c.parent = rightChild;
                }
            });
            
            root = newRoot;
            return;
        }

        final Node<T> parent = node.parent;
        ListIterator<T> valIt = parent.values.listIterator();
        ListIterator<Node<T>> refIt = parent.refs.listIterator();

        if (parent.refs.get(parent.refs.size() - 1) == node) {
            /*
             * Here is the special case that the node to split is the outermost
             * node, where the iterator would fail.
             */
            parent.refs.remove(parent.refs.size() - 1);
            parent.refs.add(leftChild);
            parent.refs.add(rightChild);
            parent.values.add(median);
        } else {
            while (valIt.hasNext() && refIt.hasNext()) {
                T curValue = valIt.next();
                Node<T> curNode = refIt.next();

                if (curNode == node) {
                    refIt.previous();
                    valIt.previous();
                    refIt.set(leftChild);
                    valIt.add(median);
                    refIt.next();
                    refIt.add(rightChild);
                    break;
                }
            }
        }

        leftChild.parent = parent;
        rightChild.parent = parent;

        leftChild.refs.stream().forEach(c -> {
            if (c != null) {
                c.parent = leftChild;
            }
        });

        rightChild.refs.stream().forEach(c -> {
            if (c != null) {
                c.parent = rightChild;
            }
        });

        /*
         * The insertion of the median into the parent node may cause the 
         * parent node to become overfull, so we have to check if we must 
         * rebalance the parent node again.
         */
        rebalance(parent);
    }

    /**
     * Navigates through the tree and returns the node where the passed 
     * {@code value} should be inserted to or that already contains this value.
     * Note that this function <em>never</em> returns {@code null}.
     * <p>
     * This traversion follows the following rules:<p>
     * Lets think about the following example node:
     *
     * <pre>
     * <code>
     *     | 1 | 2 | 3 |
     *    ^   ^^
     * </code>
     * </pre>
     *
     * So if the value is contained in the node (remember that in a B-tree also
     * final values can be contained in an internal node, not only in the leaf)
     * then this node is returned. Otherwise, the next node to search in 
     * depends on the value:
     * <ol>
     *   <li>
     *     If the value is smaller than the first element (here {@code 1}) then
     *     the next node to search in is the reference denoted with {@code ^}.
     *   </li>
     *   <li>
     *     If the value is <em>strictly</em> greater than the first element 
     *     ({@code 1}) and <em>strictly</em> smaller than the next element
     *     ({@code 2}), the next node to search in is the reference denoted
     *     with {@code ^^}.
     *   </li>
     *   <li>
     *     And so on for all references. If the value is greater than all
     *     values in the node, the next node to search in is the very last 
     *     reference in the node.
     *   </li>
     * </ol>
     *
     * @param current
     *         The current node to look in.
     * @param value
     *         The value to search in the nodes for.
     * @return Returns the node that either contains the passed value or where
     *         this value should be inserted into.
     * @throws IllegalStateException
     *         An {@code IllegalStateException} will be thrown if no node can 
     *         be detected, as this state can never occur in a B-tree.
     */
    private Node<T> get(Node<T> current, T value) {

        Node<T> node = null;
        ListIterator<Node<T>> refIt = current.refs.listIterator();

        if (!refIt.hasNext()) {
            /* If there are no child references, this is the wanted node. */
            return current;
        }

        /* 
         * If there are child references we find the node by traversing as 
         * described in the JavaDoc documentation of this function.
         */
        for (Iterator<T> it = current.values.iterator(); it.hasNext();) {
            T curValue = it.next();
            Node<T> child = refIt.next();

            if (curValue != null) {
                if (value.compareTo(curValue) < 0) {
                    if (child != null) {
                        return get(child, value);
                    } else {
                        node = current;
                        break;
                    }
                } else if ((value.compareTo(curValue) > 0)
                        && (refIt.nextIndex() == current.values.size())) {
                    Node<T> n = current.refs.get(current.refs.size() - 1);

                    if (n != null) {
                        return get(n, value);
                    } else {
                        node = current;
                        break;
                    }
                } else if (value.compareTo(curValue) == 0) {
                    /* Equality, we found the value itself. */
                    node = current;
                    break;
                }
            }
        }

        if (node == null) {
            throw new IllegalStateException("No node detected.");
        }

        return node;
    }

    public boolean remove(T value) {

        if (isEmpty()) {
            return false;
        }

        final Node<T> node = get(root, value);

        if (!node.values.contains(value)) {
            return false;
        }

        final Node<T> parent = node.parent;

        if (node.isLeaf()) {
            node.values.remove(value);
            
            if (node.underflow()) {
                rebalanceAfterDeletion(node);
            }
        } else {
            // TODO: implement deleting a non-leaf node
        }

        return true;
    }

    private void rebalanceAfterDeletion(Node<T> node) {
        assert node != null : "Null node passed.";

        if (!node.underflow()) {
            return;
        }

        final Node<T> parent = node.parent;
        Node<T> leftSibling = null, rightSibling = null;
        int leftSiblingIndex = -1, rightSiblingIndex = -1;
        ListIterator<Node<T>> refIt = parent.refs.listIterator();

        T separator = null;

        if (parent.refs.get(parent.refs.size() - 1) == node) {
            /*
             * Here is the special case that the current node is the outermost
             * node of its parent.
             */
            leftSiblingIndex = parent.refs.size() - 2;
            rightSiblingIndex = parent.refs.size() - 1;
            leftSibling = parent.refs.get(leftSiblingIndex);
            rightSibling = parent.refs.get(rightSiblingIndex);
            separator = parent.values.get(parent.values.size() - 1);
        } else {
            while (refIt.hasNext()) {
                final Node<T> curNode = refIt.next();

                if (curNode == node) {
                    final int leftIndex = refIt.previousIndex();
                    final int rightIndex = refIt.nextIndex();

                    if ((leftIndex > -1) && (leftIndex < parent.refs.size())) {
                        leftSiblingIndex = leftIndex;
                        leftSibling = parent.refs.get(leftSiblingIndex);
                        separator = parent.values.get(leftSiblingIndex);
                    }

                    if ((rightIndex > 0) 
                            && (rightIndex - 1 < parent.refs.size())) {
                        rightSiblingIndex = rightIndex;
                        rightSibling = parent.refs.get(rightSiblingIndex);

                        if (separator == null) {
                            separator 
                                = parent.values.get(rightSiblingIndex - 1);
                        }
                    }

                    break;
                }
            }
        }

        if ((rightSibling != null) && (rightSibling.values.size() > degree)) {
            // TODO: implement left rotation
            //System.out.println("rotate left");
        } else if ((leftSibling != null) && (leftSibling.values.size() > degree)) {
            // TODO: implement right rotation
            //System.out.println("rotate right");
        } else if ((leftSibling != null) && (rightSibling != null)) {
            leftSibling.values.add(separator);
            leftSibling.values.addAll(rightSibling.values);
            leftSibling.refs.addAll(rightSibling.refs);
            parent.values.remove(rightSiblingIndex - 1);
            parent.refs.remove(rightSiblingIndex);
            rightSibling = null;

            if ((parent == root) && (parent.refs.isEmpty())) {
                root = leftSibling;
            } else if (parent.underflow()) {
                rebalanceAfterDeletion(parent);
            }
        }
    }

    /**
     * Returns a string representation of the tree by printing the root.
     *
     * @return Returns a string representation of the tree if the tree is not
     *         empty, or {@code (empty tree)} otherwise.
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
     * Implementation of a single node of a {@code BTree}.<p>
     * Such a node contains exactly three things:
     *
     * <ol>
     *   <li>The list of values.</li>
     *   <li>The list of children.</li>
     *   <li>A pointer to the parent node.</li>
     * </ol>
     *
     * @author Thomas Lang
     * @version 1.1, 2015-08-03
     */
    class Node<T extends Comparable<T>> {

        /** List holding the values to the child nodes. */
        ArrayList<T> values;

        /** List holding references to child nodes. */
        ArrayList<Node<T>> refs;

        /** Reference to the parent node. */
        Node<T> parent;

        /**
         * Creates a new node by initializing the lists containing the values
         * of the node, or the children respectively.
         */
        Node() {
            values = new ArrayList<>(2 * degree);
            refs = new ArrayList<>(2 * degree + 1);
        }

        /**
         * Checks if the node is full.<p>
         * A node is full, if it already contains more than the maximum amount
         * of values, which is exactly two times the degree of the tree, as
         * described above.
         *
         * @return Returns either {@code true} if the node is full or 
         *         {@code false} otherwise.
         * @see BTree#BTree()
         */
        boolean isFull() {
            return values.size() > 2 * degree;
        }

        /**
         * Checks if this node is a leaf node.<p>
         * A node is a leaf node, if it has no children.
         *
         * @return Returns either {@code true} if the node is a leaf node or
         *         {@code false} otherwise.
         */
        boolean isLeaf() {
            ArrayList<Node<T>> refCopy = new ArrayList<Node<T>>();
            refCopy.addAll(refs);
            refCopy.removeIf(c -> c == null);
            return refCopy.isEmpty();
        }

        /**
         * Checks if this node has an underflow, what means that it contains 
         * less values than the minimum amount of entries (the degree);
         *
         * @return Returns either {@code true} if the node has an underflow or
         *         {@code false} otherwise.
         */
        boolean underflow() {
            return values.size() < degree;
        }

        /**
         * Returns a string representation of a single node.<p>
         * This representation includes firstly the values of this node itself.
         * Secondly, all children are included enclosed by parentheses.
         * This means that the entire tree can be printed by printing the root.
         * <p>
         * Let's figure this out on a small example tree:
         *
         * <pre>
         * <code>
         *         |2|
         *       /     \
         *   |0|1|     |3|
         * </code>
         * <pre>
         *
         * This tree will be printed as: {@code [2]([0,1])([3])}
         *
         * @return Returns a string representation as described above.
         */
        @Override
        public String toString() {
            StringBuilder repr = new StringBuilder();
            repr.append(values);

            for (Iterator<Node<T>> it = refs.iterator(); it.hasNext();) {
                Node<T> node = it.next();

                if (node != null) {
                    repr.append("(");
                    repr.append(node.toString());
                    repr.append(")");
                }
            }

            return repr.toString();
        }
    }

    /**
     * Main (testing) function.
     *
     * @param args
     *         Command line arguments (not used here).
     */
    public static void main(String[] args) {
        System.out.print("Creating two new B-Trees ... ");
        BTree<Integer> tree = new BTree<>(1);
        BTree<Integer> tree2 = new BTree<>(1);
        System.out.println("done.");

        for (int i = 0; i < 11; ++i) {
            tree.insert(i + 1);
        }

        tree2.insert(1);
        tree2.insert(10);
        tree2.insert(15);
        tree2.insert(-3);
        tree2.insert(8);
        tree2.insert(999);
        tree2.insert(7);
        tree2.insert(12);
        tree2.insert(144);

        System.out.println("Tree 1: " + tree);
        System.out.println("Tree 2: " + tree2);

        System.out.print("\nDeleting '9' from tree ... ");
        boolean delete9 = tree.remove(9);
        System.out.println("done, " + (delete9 ? "" : "not ") + "successfully.");
        
        System.out.println("\nTree 1 now: " + tree);
    }
}
