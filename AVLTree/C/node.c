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

/*
 * Creates a new Node with the passed {@code value}.
 *
 * @param value
 *          The value hold by the newly created node.
 */
Node* newNode(int value) {
    Node *node   = (Node*) malloc(sizeof(Node));
    node->value  = value;
    node->left   = NULL;
    node->right  = NULL;
    node->parent = NULL;
    return node;
}

/*
 * Returns the height of this node in the surrounding tree.
 *
 * @return Returns the height of this node in the surrounding tree.
 */
static int __getHeight(Node *node) {
    assert (node != NULL);

    if (!(node->left) && !(node->right)) {
        return 1;
    }

    int rh = !node->left  ? 0 : __getHeight(node->left);
    int lh = !node->right ? 0 : __getHeight(node->right);
    return 1 + MAX(lh, rh);
}

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
int getBalance(Node *node) {
    assert (node != NULL);

    if (!(node->left) && !(node->right)) {
        return 0;
    }

    int lbal = !node->left  ? 0 : __getHeight(node->left);
    int rbal = !node->right ? 0 : __getHeight(node->right);
    return rbal - lbal;
}

/*
 * Prints out this node including all children.
 *
 * @param node
 *          The node to print.
 */
void printNode(Node *node) {
    assert (node != NULL);

    if (node->left) {
        printf("(");
        printNode(node->left);
        printf(")");
    }

    printf("[%d]", node->value);

    if (node->right) {
        printf("(");
        printNode(node->right);
        printf(")");
    }
}
