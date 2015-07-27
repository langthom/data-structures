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

#ifndef __AVL_NODE__
#define __AVL_NODE__

#include <cstddef>

/*
 * Implementation of a single node of an AVL tree.<p>
 * Such a node holds a single value and references to both child nodes and
 * to its parent node. Furthermore, such a node has a balance, that depends
 * on the nodes' height in the tree. This balance value is essential for
 * tree rotations.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-07-25
 *
 * @param <T> Type parameter, where every type can be set in as long as 
 *            comparisons are well-defined.
 * @see AVLTree#rotate(Node, boolean)
 * @see #getBalance()
 */
template <typename T>
class Node {
    /*
     * Note that you can find the documentation for the declared only methods
     * in the implementation file.
     */

    public:
        /*
         * Creates a new node with the passed value.
         *
         * @param val
         *         The value of this node.
         */
        Node<T>(T val) : value(val), left(NULL),
                         right(NULL), parent(NULL) {}

        /*
         * Destructor for this node.
         */
        ~Node<T>() {
            left = NULL;
            right = NULL;
            parent = NULL;
        }

        int getBalance();
        int getHeight();
        void print();

        /*
         * Sets the 'left' pointer to the passed {@code node}.
         *
         * @param node
         *          The new 'left' reference.
         */
        void setLeft(Node<T> *node) { left = node; }

        /*
         * Sets the 'right' pointer to the passed {@code node}.
         *
         * @param node
         *          The new 'right' reference.
         */
        void setRight(Node<T> *node) { right = node; }
        
        /*
         * Sets the 'parent' pointer to the passed {@code node}.
         *
         * @param node
         *          The new 'parent' reference.
         */
        void setParent(Node<T> *node) { parent = node; }
        
        /*
         * Sets the value of this node to the passed one.
         *
         * @param value
         *          The new value of this node.
         */
        void setValue(T value) { this->value = value; }
        
        /*
         * Returns the reference to this nodes' left child.
         *
         * @return Returns the reference to this nodes' left child.
         */
        Node<T> *getLeft() { return left; }
        
        /*
         * Returns the reference to this nodes' right child.
         *
         * @return Returns the reference to this nodes' right child.
         */
        Node<T> *getRight() { return right; }
        
        /*
         * Returns the reference to this nodes' parent node.
         *
         * @return Returns the reference to this nodes' parent node.
         */
        Node<T> *getParent() { return parent; }
        
        /*
         * Returns the value of this node.
         *
         * @return Returns the value of this node.
         */
        T getValue() { return value; }
    
    private:
        /* Value of this node. */
        T value;

        /* Pointer to its left child. */
        Node<T> *left;

        /* Pointer to its right child. */
        Node<T> *right;

        /* Pointer to its parent node. */
        Node<T> *parent;
};

#include "node.tpp"

#endif
