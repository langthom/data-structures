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

#ifndef __RED_BLACK_NODE__
#define __RED_BLACK_NODE__

#define MAX(x, y) ((x < y) ? y : x)

#define TO_STR(x) (enum_strings[x])

/* Enum representing the two possible colors of a node. */
enum Color { RED, BLACK };

/* Array that maps the above enum values to strings. */
static char* enum_strings[] = { "RED", "BLACK" };

/*
 * Implementation of a single node of a Red-black tree.<p>
 * This is basically a simple node of any binary search tree but with the
 * additional {@code color} attribute of type {@code Color}, what can be
 * either {@code Color.RED} or {@code Color.BLACK}.
 *
 * @author Thomas Lang
 * @version 2015-08-09
 * @see Color
 */
typedef struct __Node__ {
    /* Single value hold by this node. */
    int value;
    /* Color of this node. */
    enum Color color;
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
