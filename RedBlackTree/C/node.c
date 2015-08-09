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
 * Creates a new Node with the passed {@code value}. Such a node is per default
 * red.
 *
 * @param value
 *          The value hold by the newly created node.
 */
Node* newNode(int value) {
    Node *node   = (Node*) malloc(sizeof(Node));
    node->value  = value;
    node->color  = RED;
    node->left   = NULL;
    node->right  = NULL;
    node->parent = NULL;
    return node;
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

    printf("[%s: %d]", TO_STR(node->color), node->value);

    if (node->right) {
        printf("(");
        printNode(node->right);
        printf(")");
    }
}
