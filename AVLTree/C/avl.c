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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "node.h"
#include "avl.h"

/*
 * Creates a new AVL tree by allocating the needed memory and initializing
 * the attributes.
 *
 * @return The newly created tree.
 */
AVLTree *newAVL() {
    AVLTree *tree = (AVLTree*) malloc(sizeof(AVLTree));
    tree->size = 0;
    tree->root = NULL;
    return tree;
}

/*
 * Deletes all nodes of the tree excluding the root.
 *
 * @param tree
 *          The tree to delete from.
 * @param node
 *          The current node used for traversing.
 */
static void __deleteFromRoot(AVLTree *tree, Node *node) {
    assert (tree != NULL);

    if (isEmpty(tree)) return;

    assert (node != NULL);

    if (node->left)  __deleteFromRoot(tree, node->left);
    if (node->right) __deleteFromRoot(tree, node->right);
    if ((node != tree->root) && !(node->left) && !(node->right)) free(node);
}

/*
 * Frees the tree by deleting all nodes and freeing the pointer itself.
 *
 * @param tree
 *          The tree to free.
 */
void freeAVL(AVLTree *tree) {
    assert (tree != NULL);

    __deleteFromRoot(tree, tree->root);
    free(tree);
}

/*
 * Clears the tree.
 *
 * @param tree
 *          The tree to clear.
 */
void clear(AVLTree *tree) {
    assert (tree != NULL);

    __deleteFromRoot(tree, tree->root);
    tree->root = NULL;
    tree->size = 0;
}

/*
 * Prints the tree to stdout.
 *
 * @param tree
 *          The tree to print.
 */
void printAVL(AVLTree *tree) {
    assert (tree != NULL);

    if (isEmpty(tree)) {
        printf("(empty tree)\n");
    } else {
        printNode(tree->root);
    }
}

/*
 * Checks if the tree contains a node with the value {@code value}.
 *
 * @param tree
 *         The tree that eventually contains a node with the value.
 * @param value
 *         The value of the node to check.
 * @return Returns either {@code true} if the tree contains such a node, or
 *         {@code false} if not.
 */
int contains(AVLTree *tree, int value) {
    assert (tree != NULL);

    return get(tree, value) != NULL;
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
static int __insert(Node *current, Node *node) {
    assert (current != NULL);
    assert (node    != NULL);

    int __newValue = node->value;
    int __curValue = current->value;

    if (__newValue < __curValue) {
        if (!(current->left)) {
            current->left = node;
            node->parent = current;
        } else {
            return __insert(current->left, node);
        }
    } else if (__newValue > __curValue) {
        if (!(current->right)) {
            current->right = node;
            node->parent = current;
        } else {
            return __insert(current->right, node);
        }
    } else {
        /* No duplicates allowed. */
        return FALSE;
    }

    return TRUE;
}

/*
 * Performs a simple 'left' rotation around the node {@code node}.
 *
 * @param tree
 *          The tree where the rotation takes place in.
 * @param node
 *          The node to rotate around, which should not be {@code null}.
 */
static void __rotateLeft(AVLTree *tree, Node *node) {
    assert (tree != NULL);
    assert (node != NULL);

    Node *__parent    = node->parent;
    Node *__right     = node->right;
    Node *__rightLeft = __right->left;

    node->parent      = __right;
    node->right       = __rightLeft;

    __right->parent   = __parent;
    __right->left     = node;

    if (__rightLeft) {
        __rightLeft->parent = node;
    }

    if (!__parent) {
        tree->root = __right;
    } else if (__parent->left == node) {
        __parent->left = __right;
    } else {
        __parent->right = __right;
    }
}

/*
 * Performs a simple 'right' rotation around the node {@code node}.
 *
 * @param tree
 *          The tree where the rotation takes place in.
 * @param node
 *          The node to rotate around, which should not be {@code null}.
 */
static void __rotateRight(AVLTree *tree, Node *node) {
    assert (tree != NULL);
    assert (node != NULL);

    Node *__parent    = node->parent;
    Node *__left      = node->left;
    Node *__leftRight = __left->right;

    node->parent      = __left;
    node->left        = __leftRight;

    __left->parent    = __parent;
    __left->right     = node;

    if (__leftRight) {
        __leftRight->parent = node;
    }

    if (!__parent) {
        tree->root = __left;
    } else if (__parent->right == node) {
        __parent->right = __left;
    } else {
        __parent->left = __left;
    }
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
 * @param tree
 *          The tree where the rotation takes place in.
 * @param node
 *          The node to rotate over, which should not be {@code null}.
 * @param deletion
 *          Indicator if this is a rotation after an insertion or after
 *          a deletion of the node.
 * @see <a href="https://en.wikipedia.org/wiki/AVL_tree#Insertion">
 *      Insertions and Deletions in AVL trees on Wikipedia
 *      </a>
 */
static void __rotate(AVLTree *tree, Node *node, int deletion) {
    assert (tree != NULL);
    assert (node != NULL);

    if ((node == tree->root) && !(node->left) && !(node->right)) {
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
    Node *__parent = deletion ? node : node->parent;
    int __balance = (!__parent) ? 0 : getBalance(__parent);

    if (__balance == NO_CHANGE) {
        if (deletion && __parent->parent) {
            __rotate(tree, node->parent, deletion);
        } else {
            return;
        }
    } else if (ABS(__balance) == REBAL_ONE_POS) {
        if (!deletion && __parent->parent) {
            __rotate(tree, node->parent, deletion);
        } else if (deletion) {
            return;
        }
    } else {
        int __lbal = (!__parent->left) ? 0 : getBalance(__parent->left);
        int __rbal = (!__parent->right) ? 0 : getBalance(__parent->right);

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
        if (__balance == REBAL_TWO_POS) {
            if (__rbal == REBAL_ONE_POS) {
                __rotateLeft(tree, __parent);
            } else if (__rbal == REBAL_ONE_NEG) {
                __rotateRight(tree, __parent->right);
                __rotateLeft(tree, __parent);
            }
        } else if (__balance == REBAL_TWO_NEG) {
            if (__lbal == REBAL_ONE_POS) {
                __rotateLeft(tree, __parent->left);
                __rotateRight(tree, __parent);
            } else if (__lbal == REBAL_ONE_NEG) {
                __rotateRight(tree, __parent);
            }
        }
    }
}

/*
 * Inserts a new node with a value of {@code value} into this AVL tree.<p>
 * The insertion itself is just as like the insertion into a normal binary
 * search tree, but after this was successfully performed, a tree rotation
 * is made if necessary, so the tree is in balance again.
 *
 * @param tree
 *         The tree to insert to.
 * @param value
 *         The value of the new node to insert.
 * @return Returns {@code true} if the insertion was successful, 
 *         {@code false} otherwise.
 * @see rotate(Node<T>, boolean)
 */
int insert(AVLTree *tree, int value) {
    assert (tree != NULL);

    Node *node = newNode(value);
    int __success = TRUE;

    if (isEmpty(tree)) {
        tree->root = node;
    } else {
        __success = __insert(tree->root, node);
    }

    if (__success) {
        ++(tree->size);
    }

    __rotate(tree, node, FALSE);
    return __success;
}

/*
 * Checks if the tree is empty (if it has no elements).
 *
 * @param tree
 *         The tree to check if empty.
 * @return Returns either {@code true} if the tree is empty, {@code false}
 *         otherwise.
 */
int isEmpty(AVLTree *tree) {
    assert (tree != NULL);
    return tree->size == 0;
}

/*
 * Deletes a {@code node} from the tree, mainly leaf nodes.
 *
 * @param node
 *          The node to delete, which should not be {@code null}.
 */
static void __deleteNode(Node *node) {
    assert (node != NULL);

    Node *__parent = node->parent;
    Node *__left   = node->left;
    Node *__right  = node->right;
    Node *__pright = __parent->right;
    int __rightChild = __pright && (__pright->value == node->value);

    /*
     * Please note that we need not to check if 'parent' is null here,
     * because this can only happen if 'node' is the root, but this special
     * case is already recognized in the methode 'remove'.
     */

    if (!(node->left) && !(node->right)) {
        if (__rightChild) __parent->right = NULL;
        else __parent->left = NULL;
    } else if (node->left) {
        if (__rightChild) __parent->right = __left;
        else __parent->left = __left;
        
        __left->parent = __parent;
    } else {
        if (__rightChild) __parent->left = __right;
        else __parent->right = __right;

        __right->parent = __parent;
    }

    node->parent = NULL;
    free(node);
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
static Node *__findMaxOfMinNode(Node *node) {
    assert (node != NULL);

    Node *__leftChild = node->left;
    while (__leftChild->right) __leftChild = __leftChild->right;
    return __leftChild;
}

/*
 * Deletes a node internal to the tree (no leaf) by swapping it with the
 * node with a maximum value but still below the value of the {@code node}.
 *
 * @param node
 *          The internal node to delete which should not be {@code null}.
 */
static void __deleteInternalNode(Node *node) {
    assert (node != NULL);

    Node *__maxOfMin = __findMaxOfMinNode(node);
    if (!__maxOfMin) return;
    node->value = __maxOfMin->value;
    __deleteNode(__maxOfMin);
}

/*
 * Removes the node with the passed {@code value} from the tree if
 * existent. As a speciality of an AVL tree, this operation also runs
 * in {@code O(log n)} with {@code n} denoting the number of nodes in the
 * tree. Furthermore, even after a deletion of a node the tree will be 
 * rotated if necessary.
 *
 * @param tree
 *         The tree to remove the node from.
 * @param value
 *         The value of the node to delete.
 * @return Returns either {@code true} if the node was successfully deleted
 *         or {@code false} otherwise.
 */
int removeNode(AVLTree *tree, int value) {
    assert (tree != NULL);

    if (isEmpty(tree)) {
        return FALSE;
    }

    Node *__deletableNode = get(tree, value);

    if (!__deletableNode) {
        /* Node with 'value' found. */
        return FALSE;
    } else {
        Node *__parent = __deletableNode->parent;

        if (value == tree->root->value) {
            __deleteInternalNode(tree->root);
        } else if (__deletableNode->left && __deletableNode->right) {
            __deleteInternalNode(__deletableNode);
        } else {
            __deleteNode(__deletableNode);
        }

        --(tree->size);

        if (__parent) {
            __rotate(tree, __parent, TRUE);
        }

        return TRUE;
    }
}

/*
 * Returns the total amount of nodes in the tree.
 *
 * @param tree
 *         The tree to determine its size.
 * @return Returns the total amount of nodes in the tree.
 */
int size(AVLTree *tree) {
    assert (tree != NULL);
    return tree->size;
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
static Node *__get(Node *current, int value) {
    assert (current != NULL);

    int __curValue = current->value;

    if (value == __curValue) {
        /* We found the node successfully! */
        return current;
    } else if ((value < __curValue) && current->left) {
        return __get(current->left, value);
    } else if ((value > __curValue) && current->right) {
        return __get(current->right, value);
    } else {
        /* We could not find anything matching. */
        return NULL;
    }
}

/*
 * Retrieves the node with the passed {@code value} from the tree.
 *
 * @param tree
 *         The tree to get a node from.
 * @param value
 *         The value of the node to return.
 * @return Returns the node with the passed {@code value} from the tree,
 *         or {@code null} if either the tree is empty (has no elements) or
 *         if there is no such node in the tree.
 */
Node *get(AVLTree *tree, int value) {
    if (isEmpty(tree)) {
        return NULL;
    } else {
        return __get(tree->root, value);
    }
}

