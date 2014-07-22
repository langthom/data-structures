/* Implementation of all functions from "max_heap.h".
 * (C) Thomas Lang, 2014
 *
 * Here is the implementation. An extensive documentation can be
 * found in the header "max_heap.h".
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>
#include <stdlib.h>

#include "max_heap.h"

//------------------ Node-specific functions -----------------------
Node *newNode( int value )
{
    Node *node   = malloc( sizeof( Node ));
    node->value  = value;
    node->left   = NULL;
    node->right  = NULL;
    node->parent = NULL;
    return node;
}

int getValue( Node *node )
{
    return node->value;
}

void setValue( Node *node, int value )
{
    node->value = value;
}

Node *getLeft( Node *node )
{
    return node->left;
}

void setLeft( Node *node, Node *left )
{
    node->left = left;
}

Node *getRight( Node *node )
{
    return node->right;
}

void setRight( Node *node, Node *right )
{
    node->right = right;
}

Node *getParent( Node *node )
{
    return node->parent;
}

void setParent( Node *node, Node *parent )
{
    node->parent = parent;
}


//------------------ MaxHeap - specific functions -----------------

MaxHeap *newMaxHeap()
{
    MaxHeap *heap = malloc( sizeof( MaxHeap ));
    heap->root = NULL;
    heap->size = 0;
    return heap;
}

static Node *last = NULL; // pointer to last element added

int isEmpty( MaxHeap *heap )
{
    if( heap->root == NULL )
        return 0;
    return (-1);
}

int getSize( MaxHeap *heap )
{
    return heap->size;
}

void increaseSize( MaxHeap *heap )
{
    heap->size++;
}

void decreaseSize( MaxHeap *heap )
{
    heap->size--;
}

Node *getRoot( MaxHeap *heap )
{
    return heap->root;
}

void setRoot( MaxHeap *heap, Node *node )
{
    heap->root = node;
}

Node *getLast()
{
    return last;
}

void setLast( Node *node )
{
    last = node;
}

int isFullTree( Node *node )
{
    if(( node != NULL ) && ( getLeft( node ) != NULL ) && ( getRight( node ) != NULL ))
        return 0;
    return (-1);
}

void swapNodes( Node *child, Node *parent )
{
    int tmp = getValue( child );
    setValue( child, getValue( parent ));
    setValue( parent, tmp);
}

void bubbleUp( Node *node )
{
    if( getParent( node ) == NULL )
        return;
    if( getValue( getParent( node )) < getValue( node ) )
    {
        swapNodes( node, getParent( node ));
        bubbleUp( getParent( node ));
    }
}

void bubbleDown( Node *node )
{
    if(( getLeft( node ) == NULL ) && ( getRight( node ) == NULL ))
        return;
    if(( getLeft( node ) != NULL ) && ( getRight( node ) != NULL ))
    {
        if(( getValue( node ) < getValue( getLeft( node ))) &&
           ( getValue( getLeft( node )) > getValue( getRight( node )) ))
        {
            swapNodes( node, getLeft( node ));
            bubbleDown( getLeft( node ));
        }
        else if( getValue( node ) < getValue( getRight( node )))
        {
            swapNodes( node, getRight( node ));
            bubbleDown( getRight( node ));
        }
    }

    if(( getLeft( node ) != NULL ) && ( getValue( node ) < getValue( getLeft( node ))))
    {
        swapNodes( node, getLeft( node ));
        bubbleDown( getLeft( node ));
    }
    
    if(( getRight( node ) != NULL ) && ( getValue( node ) < getValue( getRight( node ))))
    {
        swapNodes( node, getRight( node ));
        bubbleDown( getRight( node ));
    }
}


void insertRec( Node *node, Node *newNode )
{
    if( node == NULL )
    {
        node = newNode;
        setLast( newNode );
    }
    else if( getLeft( node ) == NULL )
    {
        setLeft( node, newNode );
        setParent( newNode, node );
        setLast( newNode );

    }
    else if( getRight( node ) == NULL )
    {
        setRight( node, newNode );
        setParent( newNode, node );
        setLast( newNode );

    }
    else if(( isFullTree( node ) == 0 ) && ( isFullTree( getLeft( node )) == 0 ))
    {
        insertRec( getRight( node ), newNode );
    }
    else if( isFullTree( node ) == 0 )
    {
        insertRec( getLeft( node ), newNode );
    }
}

void insertNode( MaxHeap *heap, Node *node )
{
    if( isEmpty( heap ) == 0 )
    {
        setRoot( heap, node );
        setLast( node );
    }
    else
    {
        insertRec( getRoot( heap ), node );
    }
    increaseSize( heap );
}

void insert( MaxHeap *heap, int value )
{
    Node *node = newNode( value );
    insertNode( heap, node );
    bubbleUp( node );
}


void deleteMax( MaxHeap *heap )
{
    if( isEmpty( heap ) == 0 )
    {
        setRoot( heap, NULL );
        decreaseSize( heap );
        return;
    }
    Node *last = getLast( heap );
    setValue( getRoot( heap ), getValue( last ));
    // deleting "last" node
    if( getLeft( getParent( last )) == last )
    {
        setLeft( getParent( last ), NULL );
        setParent( last, NULL );
    }
    else
    {
        setRight( getParent( last ), NULL );
        setParent( last, NULL );
    }
    decreaseSize( heap );
    bubbleDown( getRoot( heap ));
}


void printRec( Node *node )
{
    if( getLeft( node ) != NULL )
        printf( "Left: %d\n", getValue( getLeft( node )));
    if( getRight( node ) != NULL )
        printf( "Right: %d\n", getValue( getRight( node )));
    if( getLeft( node ) != NULL )
        printRec( getLeft( node ));
    if( getRight( node ) != NULL )
        printRec( getRight( node ));
}

void print( MaxHeap *heap )
{
    if( isEmpty( heap ) == 0 )
        printf( "(empty heap)" );
    printf( "Root: %d\n", getValue( getRoot( heap )));
    printRec( getRoot( heap ));
}

void freeHeapRec( Node *node )
{
    if( getLeft( node ) != NULL )
        freeHeapRec( getLeft( node ));
    if( getRight( node ) != NULL )
        freeHeapRec( getRight( node ));
    if(( getLeft( node ) == NULL ) && ( getRight( node ) == NULL ))
        free( node );
}

void freeHeap( MaxHeap *heap )
{
    if( isEmpty( heap ) == 0 )
        free( heap );
    else
        freeHeapRec( getRoot( heap ));
}


