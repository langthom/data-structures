// binary_search_tree.c - implementation of a binary search tree with integers

#include <stdio.h>
#include <stdlib.h>

//--------------------------------------------------------------------
struct _Node_
{
    int value;
    struct _Node_ *left;
    struct _Node_ *right;
};

typedef struct _Node_ Node;


struct _BinTree_
{
    Node *root;
};

typedef struct _BinTree_ BinTree;

//---------------------------------------------------------------------
// methods and functions that should be used by a programmer directly,
// all the others are just for helping
Node* newNode( int element );
int isEmpty( BinTree *tree );
void insert( BinTree *tree, int element );
void printTree( BinTree *tree );
void freeTree( BinTree *tree );

//--------------------------------------------------------------------
// helper functions
void insertIntoTree( Node *node, int element );
void printTreeFromRoot( Node *node );
void freeTreeRec( Node *node );

//---------------------------------------------------------------------
/* Creating a new node that holds a passed integer value.
 *
 * @param   element  value to store in the node
 * @return  The newly generated node.
 */
Node* newNode( int element )
{
    Node *new_node_  = malloc( sizeof( Node ));
    new_node_->value = element;
    new_node_->left  = NULL;
    new_node_->right = NULL;
    return ( new_node_ );
}


//----------------------------------------------------------------------
/* Checks, whether a tree is empty or not.
 *
 * @param  tree  The tree to check.
 * @return 1, if it is empty, 0 otherwise
 */
int isEmpty( BinTree *tree )
{
    if( tree->root == NULL )
        return -1;
    return 0;
}


//-----------------------------------------------------------------------
/* Inserts a new node with value element into the tree.
 *
 * @param  tree     The tree to insert to
 * @param  element  The content of the new node 
 */
void insert( BinTree *tree, int element )
{
    if( isEmpty( tree ) )
        tree->root = newNode( element );
    else
        insertIntoTree( tree->root, element );
}


//------------------------------------------------------------------------
/* Helper-method for inserting a new node, runs recursively
 *
 * @param  node     referring point 
 * @param  element  content of the new node
 */
void insertIntoTree( Node *node,  int element )
{
    if( element < node->value )
    {
        if( node->left == NULL )
            node->left = newNode( element );
        else
            insertIntoTree( node->left, element );
    }
    else if( element > node->value )
    {
        if( node->right == NULL )
            node->right = newNode( element );
        else
            insertIntoTree( node->right, element );
    }
    else
        return;     // do nothing for not-inserting duplicates
}


//------------------------------------------------------------------------
/* Prints a passed tree to stdout.
 *
 * @param  tree  The tree to print.
 */
void printTree( BinTree *tree )
{
    if( isEmpty( tree ) )
        printf( "(empty tree)" );
    else
    {
        printf( "Root: %d\n", tree->root->value );
        printTreeFromRoot( tree->root );
        printf( "\n" );
    }
}


//-------------------------------------------------------------------------
/* Helper method for printing a tree to stdout, 
 * runs recursively in preorder.
 *
 * @param  node  referring node
 */
void printTreeFromRoot( Node *node )
{
    if( node->left != NULL )
        printf( "Left: %d", node->left->value );
    if( node->right != NULL )
        printf( "\tRight: %d\n", node->right->value );
    if( node->left != NULL )
        printTreeFromRoot( node->left );
    if( node->right != NULL )
        printTreeFromRoot( node->right );
}


//--------------------------------------------------------------------------
/* Frees the tree, so it frees the pointers to all nodes and 
 * lastly the one to the tree itself.
 *
 * @param  tree  The tree to free
 */
void freeTree( BinTree *tree )
{
    if( isEmpty( tree ))
        free( tree );
    else
        freeTreeRec( tree->root );
}


//--------------------------------------------------------------------------
/* Frees the tree recursively by freeing all nodes
 * and running through it in postorder.
 *
 * @param  node  referring point
 */
void freeTreeRec( Node *node )
{
    if( node->left != NULL )
        freeTreeRec( node->left  );
    if( node->right != NULL )
        freeTreeRec( node->right );
    if(( node->left == NULL ) && ( node->right == NULL ))
        free( node );
}

//--------------------------------------------------------------------------
int main( int argc, char *argv[] )
{
    BinTree *test = malloc( sizeof( BinTree ));
    test->root = newNode( 10 );

    insert( test, 7  );
    insert( test, 42 );
    insert( test, 8  );
    insert( test, 3  );
    insert( test, 17 );

    printTree( test );

    freeTree( test );
    return (0);
}

