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

#include <stdio.h>

#include "avl.h"

/*
 * Main testing function.
 *
 * @param argc
 *         Command line argument counter (not used here).
 * @param argv
 *         Command line arguments (not used here).
 * @return 0 on success.
 */
int main(int argc, char *argv[]) {
    printf("Creating tree ... ");
    AVLTree *tree = newAVL();
    printf("done.\n");

    printf("Provocating rotation ... ");
    insert(tree, -42);
    insert(tree, 7);
    insert(tree, 999);
    printf("done.\n");
    printf("Actual tree:\n\n");
    printAVL(tree);

    printf("\nProvocating second rotation ... ");
    insert(tree, 10);
    insert(tree, 144);
    printf("done.\n");
    printf("Actual tree:\n\n");
    printAVL(tree);

    printf("\nProvocating third rotation and root changing ... ");
    insert(tree, 9);
    printf("done.\n");
    printf("Actual tree:\n\n");
    printAVL(tree);

    printf("\nDoes the tree contain '999'? -> ");
    const char *c = contains(tree, 999) ? "true" : "false";
    printf("%s\n\n", c);

    printf("Deleting root ... ");
    removeNode(tree, 10);
    printf("done.\n");
    printAVL(tree); printf("\n");
    printf("Deleting nodes '-42', '7' ... ");
    removeNode(tree, -42);
    removeNode(tree, 7);
    printf("done.\n\n\n");

    printf("Does the tree contain '-42'? -> ");
    c = contains(tree, -42) ? "true" : "false";
    printf("%s\n\n", c);

    printf("Actual tree:\n\n");
    printAVL(tree);

    printf("\nClearing tree ... ");
    clear(tree);
    printf("done.\n");
    printf("Tree should be empty now: %s\n", (isEmpty(tree) ? "true" : "false"));

    freeAVL(tree);
    return 0;
}
