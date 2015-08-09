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

#ifndef __RED_BLACK__
#define __RED_BLACK__

#include "node.h"

#define TRUE  1
#define FALSE 0

/*
 * Implementation of a red-black tree.<p>
 * A red-black tree is a binary search tree whose nodes can have colors (either
 * red or black). This tree is self-balancing and guarantees that all important
 * operations (like searching, inserting and removing values) run in 
 * {@code O(log n)} where {@code n} denotes the number of elements in the tree.
 * <p>
 * This rebalancing is done by guaranteeing the following rules:
 * <ul>
 *  <li>
 *    The root is always black.
 *  </li>
 *  <li>
 *    The leaf nodes (what are {@code null} pointers in reality) are black.
 *  </li>
 *  <li>
 *    If a node is red, both children are black.
 *  </li>
 *  <li>
 *    Every path from a node to its children contain the same amount of black 
 *    nodes.
 *  </li>
 * </ul>
 *
 * @author Thomas Lang
 * @version 1.0, 2015-08-09
 * @see Node
 */
typedef struct {
    /* The number of elements stored in the tree. */
    int size;
    /* The root node of the tree. */
    Node *root;
} RedBlackTree;

/*
 * For documentation of the below methods please check out 'avl.c'.
 */

RedBlackTree *newRedBlack();
void clear(RedBlackTree*);
void freeRedBlack(RedBlackTree*);
void printRedBlack(RedBlackTree*);
int contains(RedBlackTree*, int);
int insert(RedBlackTree*, int);
int isEmpty(RedBlackTree*);
int removeNode(RedBlackTree*, int);
int size(RedBlackTree*);

#endif
