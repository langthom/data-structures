// main.cpp - Testing module for a binary search tree

#include <iostream>

#include "bintree.h"

using std::cout;
using std::endl;

int main( int argc, char *argv[] )
{
    BinTree *tree = new BinTree();

    cout << "*** Initializing new integer tree ..." << endl;
    cout << "*** Adding value: 5, 7, 9, 1, 4, 0, 6, 2, 42 \n" << endl;

    tree->insert( 5 ); tree->insert( 7 ); tree->insert( 9 );
    tree->insert( 1 ); tree->insert( 4 ); tree->insert( 0 );
    tree->insert( 6 ); tree->insert( 2 ); tree->insert( 42 );

    cout << "Printing actual tree: \n" << endl;
    tree->printTree();

    cout << "\n*** Traversing:\n### Preorder-traversing: " << endl;
    tree->traversePreorder();
    cout << endl;
    cout << "### Inorder-traversing: " << endl;
    tree->traverseInorder();
    cout << endl;
    cout << "### Postorder-traversing: " << endl;
    tree->traversePostorder();
    cout << endl;

    cout << "*** Deleting: 5\n" << endl;
    tree->remove( 5 );

    cout << "*** Printing actual tree: " << endl;
    tree->printTree();

    return (0);
}

