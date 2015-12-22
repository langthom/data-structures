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
package bintree

import scala.annotation.tailrec

/**
 * Trait describing a binary search tree.
 * This is a tree like data structure with each node having exactly two children
 * that can be either other nodes or empty nodes. Each node can hold a value, 
 * that must be at least comparable.
 * 
 * A special rule of binary search trees is the fact, that all nodes being part
 * of the left subtree of a node each have a smaller value. The exact opposite is
 * guaranteed for the right subtree. As there is a strict difference in values,
 * no duplicates are allowed.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 *
 * @param E
 *          Type parameter where each type can be included as long as this type
 *          is ordered or has an ordered super type.
 */
trait BinarySearchTree[+E] {
    
    /**
     * Checks if the tree does not contain any elements.
     *
     * @return Returns true if and only if there are no elements in the tree, or
     *         false otherwise.
     */
    def isEmpty(): Boolean

    /**
     * Inserts the passed value into the binary search tree.
     * This implementation obeys the special rule that if the passed value is
     * smaller than the current value (starting at the root, of course) the 
     * insertion takes place in the left subtree only, if the value is greater
     * then the current value in the right subtree only. This further means that
     * no duplicates are possible in this tree.
     *
     * @param value 
     *         The value to insert into the tree.
     * @return Returns the updated tree with the new element on success or the
     *         old tree if nothing was inserted.
     */
    def insert[T >: E <% Ordered[T]](value: T): BinarySearchTree[T]

    /**
     * Operator for adding new elements to the tree, a shortcut for the function
     * #insert(T).
     *
     * @param value
     *         The value to insert into the tree.
     * @return Returns the updated tree with the new element on success or the
     *         old tree if nothing was update.
     * @see #insert(T)
     */
    def +[T >: E <% Ordered[T]](value: T): BinarySearchTree[T] = insert(value)

    /**
     * Removes the passed element from the tree.
     * If this element was not in the tree, nothing happens.
     * In the special case of an empty tree, also nothing happens.
     * Otherwise there are three possible cases:
     *
     * <ol>
     *  <li>
     *      The node with this value has no children at all. Then the node can
     *      be safely deleted from the tree.
     *  </li>
     *  <li>
     *      The node with this value has only on child. Then it could be simply
     *      replaced with this child.
     *  </li>
     *  <li>
     *      The node with this value has two children. To delete the value safely
     *      from the tree we first have to find the maximum of the left subtree,
     *      which will replace the current value.
     *  </li>
     * </ol>
     *
     * @param value
     *         The value to delete from the tree.
     * @return Returns the updated tree without the deleted element on success or
     *         the unmodified tree if nothing was deleted.
     */
    def remove[T >: E <% Ordered[T]](value: T): BinarySearchTree[T]

    /**
     * Operator for removing elements from the tree, a shortcut for the function
     * #remove(T).
     *
     * @param value
     *         The value to delete from the tree.
     * @return Returns the updated tree without the deleted element on success or
     *         the unmodified tree if nothing was deleted.
     * @see #remove(T)
     */
    def -[T >: E <% Ordered[T]](value: T): BinarySearchTree[T] = remove(value)

    /**
     * Performs an inorder traversal over the tree.
     * In the case of a binary search tree (just like here), this gives an ascending
     * sorted list of all values.
     *
     * @return Returns an ascending sorted list of all values in the tree.
     */
    def inorder[T >: E <% Ordered[T]](): List[T]
}

/**
 * Singleton object representing a nil node.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 * @see #BinarySearchTree[E]
 */
case object Empty extends BinarySearchTree[Nothing] {
    /** @inheritdoc */
    def isEmpty = true

    /** @inheritdoc */
    def insert[E <% Ordered[E]](value: E) = BinarySearchTree(value)

    /** @inheritdoc */
    def remove[E <% Ordered[E]](value: E) = Empty

    /** @inheritdoc */
    def inorder[E <% Ordered[E]]() = Nil

    override def toString = ""
}

/**
 * Representation of an internal node that has two children and a value.
 * 
 * @constructor The constructor creates a new node with the passed children and value.
 * @param left
 *         The left child tree of this node that might be {{{Empty}}}.
 * @param value
 *         The value of this node.
 * @param right
 *         The right child tree of this node that might be {{{Empty}}}.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 * @see #BinarySearchTree[E]
 */
case class Node[E <% Ordered[E]](left: BinarySearchTree[E], value: E, right: BinarySearchTree[E]) extends BinarySearchTree[E] {
    /** @inheritdoc */
    def isEmpty = false

    /** @inheritdoc */
    def insert[T >: E <% Ordered[T]](newvalue: T) = {
        if (newvalue < value) withLeft(left + newvalue)
        else if (newvalue > value) withRight(right + newvalue)
        else this
    }

    /** @inheritdoc */
    def remove[T >: E <% Ordered[T]](dval: T) = {
        if (dval == value) (left, right) match {
            case (Empty, Empty) => Empty
            case (Empty, _)     => right
            case (_, Empty)     => left
            case _              => val succ = getInorderPredecessor(left)
                                   withLeft(Node(left - succ, succ, right))
        } else if (dval < value) withLeft(left - dval)
        else if (dval > value) withRight(right - dval)
        else this
    }

    /** @inheritdoc */
    def inorder[T >: E <% Ordered[T]](): List[T] = left.inorder() ::: value :: right.inorder()

    /**
     * Gets the biggest element of {{{dtop}}}.
     *
     * @param dtop
     *         The tree whose biggest element we want to find.
     * @return Returns the biggest element of {{{dtop}}}.
     */
    def getInorderPredecessor[T >: E <% Ordered[T]](dtop: BinarySearchTree[T]) = dtop.inorder().last

    /**
     * Creates a new node with the original value and right child but with a new, passed left child.
     *
     * @param newleft
     *         The new left subtree of the new node.
     * @return Returns the newly created node.
     */
    def withLeft[T >: E <% Ordered[T]](newleft: BinarySearchTree[T]) = Node(newleft, value, right)

    /**
     * Creates a new node with the original value and left child but with a new, passed right child.
     * 
     * @param newright
     *         The new right subtree of the new node.
     * @return Returns the newly created node.
     */
    def withRight[T >: E <% Ordered[T]](newright: BinarySearchTree[T]) = Node(left, value, newright)

    override def toString = {
        val lstr = left match { case Empty => ""; case _ => "(" + left.toString + ")" }
        val vstr = "[" + value + "]"
        val rstr = right match { case Empty => ""; case _ => "(" + right.toString + ")" }
        lstr + vstr + rstr
    }
}

/**
 * Singleton object for specifying the apply function.
 * 
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 */
object BinarySearchTree {

    /**
     * If no arguments are given (as in a call of {{{BinarySearchTree()}}}, we simply
     * want the empty tree, so just deliver it.
     *
     * @return Returns the empty tree.
     */
    def apply[E <% Ordered[E]](): BinarySearchTree[E] = Empty

    /**
     * If arguments are given (as in a call liked {{{BinarySearchTree(1, 2)}}}, we add
     * all wanted arguments to the tree in the order they arrive.
     *
     * @param elem
     *         First passed element to add to the tree.
     * @param elems
     *         Variable number of arguments to add to the tree.
     * @return Returns the binary search tree with all elements added.
     */
    def apply[E <% Ordered[E]](elem: E, elems: E*): BinarySearchTree[E] = {
        @tailrec
        def recurse(elems: List[E], tree: BinarySearchTree[E]): BinarySearchTree[E] = elems match {
            case Nil => tree
            case _   => recurse(elems.tail, tree + elems.head)
        }

        recurse(elems.toList, Node(Empty, elem, Empty))
    }
}

