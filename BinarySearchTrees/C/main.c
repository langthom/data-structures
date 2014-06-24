// main.c - test module for a binary search tree 

#include <stdlib.h>

#include "binary_search_tree.h"


int main( int argc, char *argv[] )
{
    BinTree *tree = malloc( sizeof( BinTree ));
    tree->root = newNode( 7 );
    
    insert( tree, 42 );
    insert( tree, 4  );
    insert( tree, 12 );
    insert( tree, 0  );
    
    printTree( tree );
    freeTree( tree );
    return (0);
}
