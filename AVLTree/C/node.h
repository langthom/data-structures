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

#define MAX(x, y) ((x < y) ? y : x)

/*
 * Implementation of a single node of an AVL tree.<p>
 * Such a node holds a single value and references to both child nodes and
 * to its parent node. Furthermore, such a node has a balance, that depends
 * on the nodes' height in the tree. This balance value is essential for
 * tree rotations.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-07-29
 * @see AVLTree#rotate(Node, boolean)
 * @see #getBalance()
 */
typedef struct __Node__ {
    /* Single value hold by this node. */
    int value;
    /* Pointer to the left child of this node. */
    struct __Node__ *left;
    /* Pointer to the right child of this node. */
    struct __Node__ *right;
    /* Pointer to the parent node of this node. */
    struct __Node__ *parent;
} Node;

/* For documentation of the below functions please check out 'node.c'. */

Node *newNode();
int getBalance(Node*);
void printNode(Node*);

#endif
