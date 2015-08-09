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
#include "redblack.h"

static void __rotateLeft(RedBlackTree*, Node*);
static void __rotateRight(RedBlackTree*, Node*);
static Node *get(RedBlackTree*, int);

/*
 * Creates a new RedBlack tree by allocating the needed memory and initializing
 * the attributes.
 *
 * @return The newly created tree.
 */
RedBlackTree *newRedBlack() {
    RedBlackTree *tree = (RedBlackTree*) malloc(sizeof(RedBlackTree));
    tree->size = 0;
    tree->root = NULL;
    return tree;
}

/*
 * Deletes all nodes of the tree excluding the root.
 *
 * @param tree
 *          The tree to delete from which must not be {@code null}.
 * @param node
 *          The current node used for traversing which must not be 
 *          {@code null}.
 */
static void __deleteFromRoot(RedBlackTree *tree, Node *node) {
    assert(tree != NULL);

    if (isEmpty(tree)) return;

    assert(node != NULL);

    if (node->left)  __deleteFromRoot(tree, node->left);
    if (node->right) __deleteFromRoot(tree, node->right);
    if ((node != tree->root) && !(node->left) && !(node->right)) free(node);
}

/*
 * Frees the tree by deleting all nodes and freeing the pointer itself.
 *
 * @param tree
 *          The tree to free which must not be {@code null}.
 */
void freeRedBlack(RedBlackTree *tree) {
    assert(tree != NULL);

    __deleteFromRoot(tree, tree->root);
    free(tree);
}

/*
 * Clears the tree.
 *
 * @param tree
 *          The tree to clear which must not be {@code null}.
 */
void clear(RedBlackTree *tree) {
    assert(tree != NULL);

    __deleteFromRoot(tree, tree->root);
    tree->root = NULL;
    tree->size = 0;
}

/*
 * Prints the tree to stdout.
 *
 * @param tree
 *          The tree to print which must not be {@code null}.
 */
void printRedBlack(RedBlackTree *tree) {
    assert(tree != NULL);

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
 *         The tree that eventually contains a node with the value which must
 *         not be {@code null}.
 * @param value
 *         The value of the node to check.
 * @return Returns either {@code true} if the tree contains such a node, or
 *         {@code false} if not.
 */
int contains(RedBlackTree *tree, int value) {
    assert(tree != NULL);

    return get(tree, value) != NULL;
}

/*
 * Rebalances the tree from {@code node} if necessary.
 *
 * @param tree
 *          The tree to operate on which must not be {@code null}.
 * @param node
 *          The node to start rebalancing from which must not be {@code null}.
 */
static void __rebalance(RedBlackTree *tree, Node *node) {
    assert(tree != NULL);
    assert(node != NULL);

    Node *parent = node->parent;

    if (!parent) {
        /*
         * Case 1: The new node has no parent. This means that the new node
         * is the root and the root always must be black.
         */
        node->color = BLACK;
        return;
    }

    if (parent->color == BLACK) {
        /*
         * Case 2: Per default every new node (including this one) are red.
         * When the color of the parent node is black, then the depth of
         * black nodes is still the same and we do not have to do anything.
         */
        return;
    }

    Node *grandParent = parent->parent;
    Node *uncle = (parent == grandParent->left) ? grandParent->right 
                                                : grandParent->left;

    if (uncle && (uncle->color == RED)) {
        /*
         * Case 3: Both the uncle and the parent nodes are red. Then we 
         * restore the tree by changing the below colors what makes the
         * tree be okay locally. But now, the grand parent will be
         * problematic, so we rebalance it.
         */
        parent->color = BLACK;
        uncle->color = BLACK;
        grandParent->color = RED;
        __rebalance(tree, grandParent);
    } else {

        /*
         * Case 4: The parent node and the node itself are red and the
         * path from the grand parent to the node forms a zig-zag line.
         * Then we perform a rotation and swap positions what will result
         * in a constellation useable for the fifth case.
         * The exact rotation depends on if the node was a left or a right
         * child.
         */
        if ((node == parent->right) && (parent == grandParent->left)) {
            __rotateLeft(tree, parent);
            node = node->left;
        } else if (( node == parent->left) && (parent == grandParent->right)) {
            __rotateRight(tree, parent);
            node = node->right;
        }

        /*
         * Case 5: From this position we restore the tree by swapping
         * colors and rotations around the grand parent, depending on if
         * the node was a left or a right child.
         */
        parent = node->parent;
        grandParent = parent->parent;

        parent->color = BLACK;
        grandParent->color = RED;

        if ((node == parent->left) && (parent == grandParent->left)) {
            __rotateRight(tree, grandParent);
        } else {
            __rotateLeft(tree, grandParent);
        }
    }
}

/*
 * Performs the recursive insertion into the tree.<p>
 * This function works <em>exactly</em> like the insertion into a binary
 * search tree and because of that, its runtime lies in {@code O(log n)} 
 * where {@code n} is the number of elements in the tree.
 *
 * @param tree
 *         The tree to operate on which must not be {@code null}.
 * @param current
 *         The traversal node used for recursion, that should not be 
 *         {@code null}.
 * @param newNode
 *         The new node to insert, that should not be {@code null}.
 * @return Returns {@code true} if the insertion was successful,
 *         {@code false} otherwise.
 * @see #__rebalance(RedBlackTree*, Node*);
 */
static int __insert(RedBlackTree *tree, Node *current, Node *node) {
    assert(tree    != NULL);
    assert(current != NULL);
    assert(node    != NULL);

    int __newValue = node->value;
    int __curValue = current->value;

    if (__newValue < __curValue) {
        if (!(current->left)) {
            current->left = node;
            node->parent = current;
            __rebalance(tree, node);
        } else {
            return __insert(tree, current->left, node);
        }
    } else if (__newValue > __curValue) {
        if (!(current->right)) {
            current->right = node;
            node->parent = current;
            __rebalance(tree, node);
        } else {
            return __insert(tree, current->right, node);
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
 *          The tree where the rotation takes place in which must not be 
 *          {@code null}.
 * @param node
 *          The node to rotate around, which should not be {@code null}.
 */
static void __rotateLeft(RedBlackTree *tree, Node *node) {
    assert(tree != NULL);
    assert(node != NULL);

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
 *          The tree where the rotation takes place in which must not be 
 *          {@code null}.
 * @param node
 *          The node to rotate around, which should not be {@code null}.
 */
static void __rotateRight(RedBlackTree *tree, Node *node) {
    assert(tree != NULL);
    assert(node != NULL);

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
 * Inserts a new node with a value of {@code value} into this RedBlack tree.<p>
 * The insertion itself is just as like the insertion into a normal binary
 * search tree, but after this was successfully performed, a tree rotation
 * is made if necessary, so the tree is in balance again.
 *
 * @param tree
 *         The tree to insert to which must not be {@code null}.
 * @param value
 *         The value of the new node to insert.
 * @return Returns {@code true} if the insertion was successful, 
 *         {@code false} otherwise.
 * @see #__insert(RedBlackTree*, Node*, Node*)
 */
int insert(RedBlackTree *tree, int value) {
    assert(tree != NULL);

    Node *node = newNode(value);
    int __success = TRUE;

    if (isEmpty(tree)) {
        node->color = BLACK;
        tree->root = node;
    } else {
        __success = __insert(tree, tree->root, node);
    }

    if (__success) {
        ++(tree->size);
    }

    return __success;
}

/*
 * Checks if the tree is empty (if it has no elements).
 *
 * @param tree
 *         The tree to check if empty which must not be {@code null}.
 * @return Returns either {@code true} if the tree is empty, {@code false}
 *         otherwise.
 */
int isEmpty(RedBlackTree *tree) {
    assert(tree != NULL);
    return tree->size == 0;
}

/*
 * Rebalances the tree after a deletion.
 *
 * @param tree
 *          The tree to operate on which must not be {@code null}.
 * @param node
 *          A child of the deleted node which must not be {@code null}.
 */
static void __rebalanceAfterDeletion(RedBlackTree *tree, Node *node) {
    assert(tree != NULL);
    assert(node != NULL);

    Node *parent = node->parent;

    if (!parent) {
      /* Case 1: Problematic node is root, no rotations to made. */
      return;
    }

    Node *sibling = (node == parent->left) ? parent->right : parent->left;

    if (sibling->color == RED) {
      /*
       * Case 2: The sibling of the node is red.
       * Then invert the colors of the parent and the sibling node
       * following by performing a left / right rotation around the
       * parent node depending on if the node was a left or a right
       * child.
       */
      parent->color = RED;
        sibling->color = BLACK;

        if (node == parent->left) __rotateLeft(tree, parent);
        else __rotateRight(tree, parent);
    }

    enum Color p_color = parent->color;
    enum Color s_color = sibling->color;
    enum Color sl_color = !sibling->left ? BLACK : sibling->left->color;
    enum Color sr_color = !sibling->right ? BLACK : sibling->right->color;

    if ((p_color == BLACK) && (s_color == BLACK) 
            && (sl_color == BLACK) && (sr_color == BLACK)) {
        /*
         * Case 3: The parent, the sibling and both children of the sibling
         * are black. Then the sibling has the wrong color, so change it to
         * red. This may have corrupted any integraty conditions of the
         * parent node, so we have to rebalance the parent node.
         */
        sibling->color = RED;
        __rebalanceAfterDeletion(tree, parent);
    } else if ((p_color == RED) && (s_color == BLACK) 
            && (sl_color == BLACK) && (sr_color == BLACK)) {
        /*
         * Case 4: The sibling and its both children are black but the 
         * parent is red. Then we can rebalance the tree by simply 
         * inverting the colors of the sibling and parent node.
         */
        sibling->color = RED;
        parent->color = BLACK;
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
        if ((node == parent->left) && (s_color == BLACK)
                && (sl_color == RED) && (sr_color == BLACK)) {
            sibling->color = RED;
            sibling->left->color = BLACK;
            __rotateRight(tree, sibling);
        } else if ((node == parent->right) && (s_color == BLACK)
                && (sl_color == BLACK) && (sr_color == RED)) {
            sibling->color = RED;
            sibling->right->color = BLACK;
            __rotateLeft(tree, sibling);
        }

        /*
         * Case 6: The sibling is black, the right child of the sibling is
         * red and the node is the left child of it's parent.
         * Then we resolve this illegal state by changing the colors as
         * below. After this, we have to correct the now invalid paths
         * by rotating, depending on if the node was a left or a right 
         * child.
         */
        sibling->color = parent->color;
        parent->color = BLACK;

        if (node == parent->left) {
            sibling->right->color = BLACK;
            __rotateLeft(tree, parent);
        } else {
            sibling->left->color = BLACK;
            __rotateRight(tree, parent);
        }
    }
}

/*
 * Deletes a node from the tree that does not have two real children.
 * The tree will be rebalanced if necessary.
 *
 * @param tree
 *          The tree to operate on which must not be {@code null}.
 * @param node
 *          The node to delete which must not be {@code null}.
 * @see #rebalanceAfterDeletion(Node<T>)
 */
static void __deleteNode(RedBlackTree *tree, Node *node) {
    assert(tree != NULL);
    assert(node != NULL);

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
        if (node->color == BLACK) __rebalanceAfterDeletion(tree, node);
        if (__rightChild) __parent->right = NULL;
        else __parent->left = NULL;
    } else if (node->left) {
        if (__rightChild) __parent->right = __left;
        else __parent->left = __left;
        
        __left->parent = __parent;
    } else {
        if (__rightChild) {
            __parent->right = __right;
            __parent->right->color = node->color;
        } else {
            __parent->left = __right;
            __parent->left->color = node->color;
        }

        __right->parent = __parent;
    }

    node->parent = NULL;
    free(node);
}

/*
 * Deletes a node from the tree that has both two children.
 *
 * @param tree
 *          The tree to operate on which must not be {@code null}.
 * @param node
 *          The node to delete which must not be {@code null}.
 * @see #deleteNode(Node<T>)
 */
static void __deleteInternalNode(RedBlackTree *tree, Node *node) {
    assert(tree != NULL);
    assert(node != NULL);

    /*
     * The node is deleted by exchanging its value with the largest value
     * from the left sub tree and finally deleting the maximum node of the
     * left sub tree.
     */
    Node *__maxOfMin = node->left;
    while (__maxOfMin->right) __maxOfMin = __maxOfMin->right;
    node->value = __maxOfMin->value;
    __deleteNode(tree, __maxOfMin);
}

/*
 * Removes the passed {@code value} from this tree.<p>
 * If necessary, the tree will be rebalanced after the removal. If the 
 * passed {@code value} does not exist in the tree, this function does
 * nothing and returns {@code false}.
 *
 * @param tree
 *         The tree to operate on which must not be {@code null}.
 * @param value
 *         The value to remove from the tree.
 * @return Returns {@code true} if the removal was successful or 
 *         {@code false} otherwise.
 * @see #deleteInternalNode(Node<T>)
 * @see #deleteNode(Node<T>)
 */
int removeNode(RedBlackTree *tree, int value) {
    assert(tree != NULL);

    Node *__deletableNode = get(tree, value);

    if (!__deletableNode) {
        /* Node with 'value' found. */
        return FALSE;
    } else {
        if (value == tree->root->value) {
            if (!tree->root->left && !tree->root->right) {
                tree->root = NULL;
                return TRUE;
            } else if (tree->root->left && !tree->root->right) {
                enum Color c = tree->root->color;
                tree->root = tree->root->left;
                tree->root->color = c;
            } else if (!tree->root->left && tree->root->right) {
                enum Color c = tree->root->color;
                tree->root = tree->root->right;
                tree->root->color = c;
            } else __deleteInternalNode(tree, tree->root);
        } else if (__deletableNode->left && __deletableNode->right) {
            __deleteInternalNode(tree, __deletableNode);
        } else {
            __deleteNode(tree, __deletableNode);
        }

        --(tree->size);
        return TRUE;
    }
}

/*
 * Returns the total amount of nodes in the tree.
 *
 * @param tree
 *         The tree to determine its size which must not be {@code null}.
 * @return Returns the total amount of nodes in the tree.
 */
int size(RedBlackTree *tree) {
    assert(tree != NULL);
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
    assert(current != NULL);

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
 * Gets the node containing the passed {@code value}.
 *
 * @param tree
 *         The tree to search in which must not be {@code null}.
 * @param value
 *         The value of the node to find.
 * @return Returns either the node that contains the passed value or 
 *         {@code null} otherwise.
 * @see #__get(Node*, int)
 */
static Node *get(RedBlackTree *tree, int value) {
    assert(tree != NULL);

    if (isEmpty(tree)) return NULL;
    return __get(tree->root, value);
}

