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

#include <iostream>

#include "node.h"

#define MAX(x,y) ((x < y) ? y : x)

using std::cout;

/*
 * Returns the <em>balance</em> of this node, which is simply the
 * height of its right sub tree minored by the height of the left
 * sub tree.<p>
 * This value is essential for tree rotations that will stabilize the
 * tree after every operation if necessary.
 *
 * @return Returns the balance of this node.
 * @see AVLTree#rotate(Node, boolean)
 */
template <typename T>
int Node<T>::getBalance() {

    if ((left == NULL) && (right == NULL)) {
        return 0;
    }

    const int lh = (left == NULL) ? 0 : left->getHeight();
    const int rh = (right == NULL) ? 0 : right->getHeight();
    return rh - lh;
}

/*
 * Returns the height of this node in the surrounding tree.
 *
 * @return Returns the height of this node in the surrounding tree.
 */
template <typename T>
int Node<T>::getHeight() {

    if ((left == NULL) && (right == NULL)) {
        /*
         * Note that for calculating purposes the minimum height in a
         * tree is 1, not 0.
         */
        return 1;
    }

    const int lh = (left == NULL) ? 0 : left->getHeight();
    const int rh = (right == NULL) ? 0 : right->getHeight();
    return 1 + MAX(lh, rh);
}

/*
 * Prints out the node including all children using a special notation:<p>
 * There every child will be surrounded by parentheses, the values will be
 * denoted in brackets, e.g.:
 *
 * <pre>
 * <code>
 *   ([-42])[7]([999])
 * </code>
 * </pre>
 * 
 * This represents a tree whose root has the value {@code 7} and a left and
 * a right child, whose values are {@code -42} (left child) and {@code 999}
 * (right child).
 */
template <typename T>
void Node<T>::print() {
    if (this->left != NULL) {
        cout << "(";
        this->left->print();
        cout << ")";
    }

    cout << "[" << this->value << "]";
    
    if (this->right != NULL) {
        cout << "(";
        this->right->print();
        cout << ")";
    }
}
