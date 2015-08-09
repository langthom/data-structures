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

#include "redblack.h"

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
    RedBlackTree *tree = newRedBlack();
    printf("done.\n");

    printf("Inserting some values ... ");
    insert(tree, 7);
    insert(tree, 8);
    insert(tree, 6);
    insert(tree, 12);
    insert(tree, 0);
    insert(tree, 9);
    insert(tree, 10);
    insert(tree, -7);
    insert(tree, 999);
    printf("done.\n");
    printf("Tree now:\n");

    printRedBlack(tree);

    printf("\nDeleting a few values ... ");
    removeNode(tree, 12);
    removeNode(tree, 0);
    removeNode(tree, 9);
    removeNode(tree, 7);
    removeNode(tree, 8);
    removeNode(tree, 10);
    removeNode(tree, -7);
    removeNode(tree, 6);
    printf("done.\n");
    printf("Tree now:\n");

    printRedBlack(tree);

    printf("\nClearing tree now ... ");
    clear(tree);
    printf("done.\n");
    printf("Tree now:\n");

    printRedBlack(tree);
    freeRedBlack(tree);
    return 0;
}
