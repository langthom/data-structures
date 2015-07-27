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
#include <cassert>

#include "avl.h"
#include "node.h"

using std::cout;
using std::endl;

template <typename T> const int AVLTree<T>::NO_CHANGE = 0;
template <typename T> const int AVLTree<T>::REBAL_ONE_POS = 1;
template <typename T> const int AVLTree<T>::REBAL_ONE_NEG = -1;
template <typename T> const int AVLTree<T>::REBAL_TWO_POS = 2;
template <typename T> const int AVLTree<T>::REBAL_TWO_NEG = -2;

/*
 * Deletes all nodes beginning from {@code node}.
 *
 * @param node
 *          The node to start traversing at.
 */
template <typename T>
void AVLTree<T>::deleteTreeFromRoot(Node<T> *node) {
    if (node->getLeft() != NULL) deleteTreeFromRoot(node->getLeft());
    if (node->getRight() != NULL) deleteTreeFromRoot(node->getRight());
    if ((node != this->root) 
        && (node->getLeft() == NULL) 
        && (node->getRight() == NULL)) delete node;
}

/*
 * Destructor for a AVLTree.
 */
template <typename T>
AVLTree<T>::~AVLTree<T>() {
    deleteTreeFromRoot(this->root);
}

/*
 * Checks if the tree contains a node having the passed {@code value}.
 *
 * @param value
 *         The value of the node to search for.
 * @return Returns {@code true} if there is such a node, {@code false} 
 *         otherwise.
 */
template <typename T>
bool AVLTree<T>::contains(T value) {
    return get(value) != NULL;
}

/*
 * Inserts a new node with a value of {@code value} into this AVL tree.<p>
 * The insertion itself is just as like the insertion into a normal binary
 * search tree, but after this was successfully performed, a tree rotation
 * is made if necessary, so the tree is in balance again.
 *
 * @param value
 *           The value of the new node to insert.
 * @return Returns {@code true} if the insertion was successful, 
 *         {@code false} otherwise.
 * @see rotate(Node<T>, boolean)
 */
template <typename T>
bool AVLTree<T>::insert(T value) {
    Node<T> *node = new Node<T>(value);
    bool success = true;

    if (isEmpty()) {
        this->root = node;
    } else {
        success = insert(this->root, node);
    }

    if (success) {
        ++size;
    }

    rotate(node, false);
    return success;
}

/*
 * Performs the recursive insertion into the tree.<p>
 * This function works <em>exactly</em> like the insertion into a binary
 * search tree and because of that, its runtime lies in {@code O(log n)} 
 * where {@code n} is the number of elements in the tree.
 *
 * @param current
 *           The traversal node used for recursion, that should not be 
 *           {@code null}.
 * @param newNode
 *           The new node to insert, that should not be {@code null}.
 * @return Returns {@code true} if the insertion was successful,
 *         {@code false} otherwise.
 */
template <typename T>
bool AVLTree<T>::insert(Node<T> *current, Node<T> *node) {
    assert(node != NULL);
    assert(current != NULL);
    
    T newValue = node->getValue();
    T curValue = current->getValue();

    if (newValue < curValue) {
        if (current->getLeft() == NULL) {
            current->setLeft(node);
            node->setParent(current);
        } else {
            insert(current->getLeft(), node);
        }
    } else if (newValue > curValue) {
        if (current->getRight() == NULL) {
            current->setRight(node);
            node->setParent(current);
        } else {
            insert(current->getRight(), node);
        }
    } else {
        /* No duplicates allowed. */
        return false;
    }

    return true;
}

/*
 * Performs a tree rotation around the node {@code node}.<p>
 * This method performs a tree rotation around the passed node if 
 * necessary. A rotation must only be performed, if the balance of the
 * node reaches a certain level. The exact behaviour is different, if 
 * it's a rotation for an insertion or if it's a rotation after deleting 
 * a node.:
 * 
 * <ul>
 * <li>
 * If the rotation will be made after an insertion:
 *   <ol>
 *     <li>If the balance is zero, nothing changed to the tree, so there
 *         need not to be any rotation taken.</li>
 *     <li>If the balance is &plusmn; 1, then we must check the balance
 *         of the parent node.</li>
 *     <li>If the balance is &plusmn; 2, then we perform a rotation
 *         depending on this balance and the balance of the previous child.
 *     </li>
 *   </ol>
 * </li>
 * <li>
 * If the rotation will be made after the deletion of a node:
 *   <ol>
 *     <li>If the balance was zero, we must check the balance of the parent
 *         node.</li>
 *     <li>If the balance was &plusmn; 1, then nothing changed on the 
 *         balances of the tree, so no rotation will be performed.</li>
 *     <li>If the balance was &plusmn; 2, then we perform a rotation 
 *         depending on that balance and the one of the deleted node.</li>
 *   </ol>
 * </li>
 * </ul>
 *
 * Note that as a special property of the <em>AVL</em> tree, the balances 
 * of every node <em>always</em> lies in the range of 
 * {@code -1 &leq; x &leq; 1} with {@code x} denoting the balance.
 *
 * @param node
 *          The node to rotate over, which should not be {@code null}.
 * @param deletion
 *          Indicator if this is a rotation after an insertion or after
 *          a deletion of the node.
 * @see <a href="https://en.wikipedia.org/wiki/AVL_tree#Insertion">
 *      Insertions and Deletions in AVL trees on Wikipedia
 *      </a>
 */
template <typename T>
void AVLTree<T>::rotate(Node<T> *node, bool deletion) {
    assert(node != NULL);

    if ((node == this->root) 
        && (this->root->getLeft() == NULL) 
        && (this->root->getRight() == NULL)) {
        /*
         * If we have the pure root node here, which does not have any
         * children in this pure case, we need not to rotate anything.
         */
        return;
    }

    /*
     * Note that if we do not start with the root here, the pointer to the
     * parent node of the passed one is always not null.
     */
    Node<T> *parent = deletion ? node : node->getParent();
    int balance = (parent == NULL) ? 0 : parent->getBalance();

    if (balance == AVLTree<T>::NO_CHANGE) {
        if (deletion && (parent->getParent() != NULL)) {
            rotate(node->getParent(), deletion);
        } else {
            return;
        }
    } else if ((balance == AVLTree<T>::REBAL_ONE_POS) 
            || (balance == AVLTree<T>::REBAL_ONE_NEG)) {
        if (!deletion && (parent->getParent() != NULL)) {
            rotate(node->getParent(), deletion);
        } else if (deletion) {
            return;
        }
    } else {
        int lbal 
            = (parent->getLeft() == NULL) ? 0 : parent->getLeft()->getBalance();
        int rbal 
            = (parent->getRight() == NULL) ? 0 : parent->getRight()->getBalance();

        /*
         * Performing rotations depending on the balances of the nodes:
         *
         * (1) If the node parents balance is +2:
         *
         *   (1.1) If the balance of the right child is +1, we perform a 
         *         'left' rotation, so the right child will become the 
         *         parent of its former parent node.
         *
         *   (1.2) If the balance of the right child is -1, we perform a
         *         'right-left' rotation, what is basically a 'right'
         *         rotation on the right child followed by a 'left' 
         *         rotation on the parent node.
         *
         * (2) If the node parents balance is -2:
         *
         *   (2.1) If the balance of the left child is +1, we perform a
         *         'left-right' rotation, what is basically a 'left'
         *         rotation on the left child followed by a 'right'
         *         rotation on the parent node.
         *
         *   (2.2) If the balance of the left child is -1, we perform a
         *         'right' rotation, so the left child will become the
         *         parent of its former parent node.
         */
        if (balance == AVLTree<T>::REBAL_TWO_POS) {
            if (rbal == AVLTree<T>::REBAL_ONE_POS) {
                rotateLeft(parent);
            } else if (rbal == AVLTree<T>::REBAL_ONE_NEG) {
                rotateRight(parent->getRight());
                rotateLeft(parent);
            }
        } else if (balance == AVLTree<T>::REBAL_TWO_NEG) {
            if (lbal == AVLTree<T>::REBAL_ONE_POS) {
                rotateLeft(parent->getLeft());
                rotateRight(parent);
            } else if (lbal == AVLTree<T>::REBAL_ONE_NEG) {
                rotateRight(parent);
            }
        }
    }
}

/*
 * Performs a simple 'left' rotation around the node {@code node}.
 *
 * @param node
 *          The node to rotate around, which should not be {@code null}.
 */
template <typename T>
void AVLTree<T>::rotateLeft(Node<T> *node) {
    assert(node != NULL);

    Node<T> *parent    = node->getParent();
    Node<T> *right     = node->getRight();
    Node<T> *rightLeft = right->getLeft();

    node->setParent(right);
    node->setRight(rightLeft);

    right->setParent(parent);
    right->setLeft(node);

    if (rightLeft != NULL) {
        rightLeft->setParent(node);
    }

    if (parent == NULL) {
        this->root = right;
    } else if (parent->getLeft() == node) {
        parent->setLeft(right);
    } else {
        parent->setRight(right);
    }
}

/**
 * Performs a simple 'right' rotation around the node {@code node}.
 *
 * @param node
 *          The node to rotate around, which should not be {@code null}.
 */
template <typename T>
void AVLTree<T>::rotateRight(Node<T> *node) {
    assert(node != NULL);

    Node<T> *parent    = node->getParent();
    Node<T> *left      = node->getLeft();
    Node<T> *leftRight = left->getRight();

    node->setParent(left);
    node->setLeft(leftRight);

    left->setParent(parent);
    left->setRight(node);

    if (leftRight != NULL) {
        leftRight->setParent(node);
    }

    if (parent == NULL) {
        this->root = left;
    } else if (parent->getRight() == node) {
        parent->setRight(left);
    } else {
        parent->setLeft(left);
    }
}

/*
 * Removes the node with the passed {@code value} from the tree if
 * existent. As a speciality of an AVL tree, this operation also runs
 * in {@code O(log n)} with {@code n} denoting the number of nodes in the
 * tree. Furthermore, even after a deletion of a node the tree will be 
 * rotated if necessary.
 *
 * @param value
 *         The value of the node to delete.
 * @return Returns either {@code true} if the node was successfully deleted
 *         or {@code false} otherwise.
 */
template <typename T>
bool AVLTree<T>::remove(T value) {
    
    if (isEmpty()) {
        return false;
    }

    Node<T> *deleteableNode = get(value);

    if (deleteableNode == NULL) {
        /* No node with 'value' found. */
        return false;
    } else {
        Node<T> *parent = deleteableNode->getParent();

        if (value == this->root->getValue()) {
            deleteInternalNode(this->root);
        } else if ((deleteableNode->getLeft() != NULL)
                && (deleteableNode->getRight() != NULL)) {
            deleteInternalNode(deleteableNode);
        } else {
            deleteNode(deleteableNode);
        }

        --size;

        if (parent != NULL) {
            rotate(parent, true);
        }

        return true;
    }
}

/*
 * Deletes a node internal to the tree (no leaf) by swapping it with the
 * node with a maximum value but still below the value of the {@code node}.
 *
 * @param node
 *          The internal node to delete which should not be {@code null}.
 */
template <typename T>
void AVLTree<T>::deleteInternalNode(Node<T> *node) {
    assert(node != NULL);

    Node<T> *maxOfMin = findMaxOfMinNode(node);

    if (maxOfMin == NULL) {
        return;
    }

    node->setValue(maxOfMin->getValue());
    deleteNode(maxOfMin);
}

/*
 * Deletes a {@code node} from the tree, mainly leaf nodes.
 *
 * @param node
 *          The node to delete, which should not be {@code null}.
 */
template <typename T>
void AVLTree<T>::deleteNode(Node<T> *node) {
    assert(node != NULL);

    Node<T> *parent = node->getParent();
    Node<T> *left   = node->getLeft();
    Node<T> *right  = node->getRight();
    Node<T> *pright = parent->getRight();

    bool rightChild 
        = (pright != NULL) && (pright->getValue() == node->getValue());

    /*
     * Please note that we need not to check if 'parent' is null here,
     * because this can only happen if 'node' is the root, but this special
     * case is already recognized in the methode 'remove'.
     */

  if ((node->getLeft() == NULL) && (node->getRight() == NULL)) {
        if (rightChild) {
            parent->setRight(NULL);
        } else {
            parent->setLeft(NULL);
        }
    } else if (left != NULL) {
        if (rightChild) {
            parent->setRight(left);
        } else {
            parent->setLeft(left);
        }

        left->setParent(parent);
    } else {
        if (rightChild) {
            parent->setLeft(right);
        } else {
            parent->setRight(right);
        }

        right->setParent(parent);
    }

    node->setParent(NULL);
}

/*
 * Finds the node with the maximum value in the left sub tree of 
 * {@code node}.
 *
 * @param node
 *         The node that marks the root of the corresponding sub tree which
 *         should not be {@code null}.
 * @return Returns the node with the maximum value in the left sub tree
 *         of {@code node}.
 */
template <typename T>
Node<T> *AVLTree<T>::findMaxOfMinNode(Node<T> *node) {
    assert(node != NULL);

    Node<T> *leftChild = node->getLeft();

    while (leftChild->getRight() != NULL) {
        leftChild = leftChild->getRight();
    }

    return leftChild;
}

/*
 * Retrieves the node with the passed {@code value} from the tree 
 * recursively.
 *
 * @param current
 *           The traversal node used for recursion, which should not be
 *           {@code null}.
 * @param value
 *           The value of the node to retrieve.
 * @return Returns either {@code true} if the node with the passed 
 *         {@code value} was found successfully or {@code null} otherwise.
 */
template <typename T>
Node<T> *AVLTree<T>::get(Node<T> *current, T value) {
    assert(current != NULL);

    T curValue = current->getValue();

    if (value == curValue) {
        /* We found the node successfully. */
        return current;
    } else if ((value < curValue) && (current->getLeft() != NULL)) {
        return get(current->getLeft(), value);
    } else if ((value > curValue) && (current->getRight() != NULL)) {
        return get(current->getRight(), value);
    } else {
        /* We could not find anything matching. */
        return NULL;
    }
}

/*
 * Retrieves the node with the passed {@code value} from the tree.
 *
 * @param value
 *          The value of the node to return.
 * @return Returns the node with the passed {@code value} from the tree,
 *         or {@code null} if either the tree is empty (has no elements) or
 *         if there is no such node in the tree.
 */
template <typename T>
Node<T> *AVLTree<T>::get(T value) {
    if (isEmpty()) {
        return NULL;
    } else {
        return get(root, value);
    }
}

/*
 * Clears the tree.
 */
template <typename T>
void AVLTree<T>::clear() {
    deleteTreeFromRoot(this->root);
    size = 0;
}

