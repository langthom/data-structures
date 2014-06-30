// bintree.cpp - implementation of a binary search tree

#include <iostream>
#include <cstdlib>

#include "node.h"
#include "bintree.h"

using std::cout;
using std::endl;

// helper-functions, just for making it compile
void deleteTreeFromRoot( Node *node );
void insertFromRoot( Node *node, int element );
void traversePreorderRec( Node *node );
void traverseInorderRec( Node *node );
void traversePostorderRec( Node *node );
Node *findNodeRec( Node *node, int element );
void deleteRoot();
void deleteInternalNode( Node *node );
void deleteNode( Node *node );
void printTree();
void printTreeRec( Node *node );


//------------------------------------------------------------------------
BinTree::BinTree() :
    root( NULL ){}

//------------------------------------------------------------------------

BinTree::~BinTree()
{
    deleteTreeFromRoot( root );
}

void deleteTreeFromRoot( Node *node )
{
    if( node->getLeft() != NULL )
        deleteTreeFromRoot( node->getLeft()  );
    if( node->getRight() != NULL )
        deleteTreeFromRoot( node->getRight() );
    if(( node->getLeft() == NULL ) && ( node->getRight() == NULL ))
        delete[] node;
}

//------------------------------------------------------------------------

void BinTree::insert( int element )
{
    if( isEmpty() )
        root = new Node( element );
    else
        insertFromRoot( root, element );
}

void insertFromRoot( Node *node, int element )
{
    if( element < node->getValue() )
    {
        if( node->getLeft() == NULL )
        {
            node->setLeft( new Node( element ));
            node->getLeft()->setParent( node );
        }
        else
            insertFromRoot( node->getLeft(), element );
    }
    else if( element > node->getValue() )
    {
        if( node->getRight() == NULL )
        {
            node->setRight( new Node( element ));
            node->getRight()->setParent( node );
        }
        else
            insertFromRoot( node->getRight(), element );
    }
    else
        return; // for not inserting duplicates
}

//------------------------------------------------------------------------

void BinTree::traversePreorder()
{
    if( root != NULL )
        traversePreorderRec( root );
    cout << endl;
}

void traversePreorderRec( Node *node )
{
    cout << node->getValue() << " ";
    if( node->getLeft() != NULL )
        traversePreorderRec( node->getLeft() );
    if( node->getRight() != NULL )
        traversePreorderRec( node->getRight() );
}

//------------------------------------------------------------------------

void BinTree::traverseInorder()
{
    if( root != NULL )
        traverseInorderRec( root );
    cout << endl;
}

void traverseInorderRec( Node *node )
{
    if( node->getLeft() != NULL )
        traverseInorderRec( node->getLeft() );
    cout << node->getValue() << " ";
    if( node->getRight() != NULL )
        traverseInorderRec( node->getRight() );
}

//------------------------------------------------------------------------

void BinTree::traversePostorder()
{
    if( root != NULL )
        traversePostorderRec( root );
    cout << endl;
}

void traversePostorderRec( Node *node )
{
    if( node->getLeft() != NULL )
        traversePostorderRec( node->getLeft() );
    if( node->getRight() != NULL )
        traversePostorderRec( node->getRight() );
    cout << node->getValue() << " ";
}

//------------------------------------------------------------------------

Node *BinTree::findNode( int element )
{
    if( isEmpty() )
        cout << "Error! No elements in empty tree." << endl;
    else
        return findNodeRec( root, element );
    return NULL;
}

Node *findNodeRec( Node *node, int element )
{
    if( node->getValue() == element )
        return node;
    else if( node->getValue() < element )
        findNodeRec( node->getLeft(), element );
    else
        findNodeRec( node->getRight(), element );
    return NULL;
}

//------------------------------------------------------------------------

Node *findMaxOfMin( Node *node )
{
    Node *left_child_ = NULL;
    if( node->getLeft() != NULL )
    {
        left_child_ = node->getLeft();
        while( left_child_->getRight() != NULL )
            left_child_ = left_child_->getRight();
    }
    return left_child_;
}

//------------------------------------------------------------------------

void BinTree::remove( int element )
{
    if( isEmpty() )
    {
        cout << "Cannot delete from empty tree." << endl;
        return; 
    }
    Node *node_to_delete_ = findNode( element );
    if( node_to_delete_ != NULL )
    {
        if( node_to_delete_->getValue() == root->getValue() )
            deleteInternalNode( root );
        else if(( node_to_delete_->getLeft() != NULL ) && ( node_to_delete_->getRight() != NULL ))
            deleteInternalNode( node_to_delete_ );
        else
           deleteNode( node_to_delete_ );
    }
}

void deleteInternalNode( Node *node )
{
    Node *max_of_min_ = findMaxOfMin( node );
    node->setValue( max_of_min_->getValue() );
    deleteNode( max_of_min_ );
}

void deleteNode( Node *node )
{
    if(( node->getLeft() == NULL ) && ( node->getRight() == NULL ))
    {
        if( node->getParent()->getRight()->getValue() == node->getValue() )
        {
            node->getParent()->setRight( NULL );
            node->setParent( NULL );
        }
        else
        {
            node->getParent()->setLeft( NULL );
            node->setParent( NULL );
        }
    }
    else if( node->getLeft() != NULL )
    {
        if( node->getParent()->getRight()->getValue() == node->getValue() )
        {
            node->getParent()->setRight( node->getLeft() );
            node->getLeft()->setParent( node->getParent() );
            node->setParent( NULL );
        }
        else
        {
            node->getParent()->setLeft( node->getLeft() );
            node->getLeft()->setParent( node->getParent() );
            node->setParent( NULL );
        }
    }
    else
    {
        if( node->getParent()->getRight()->getValue() == node->getValue() )
        {
            node->getParent()->setLeft( node->getRight() );
            node->getRight()->setParent( node->getParent() );
            node->setParent( NULL );
        }
        else
        {
            node->getParent()->setRight( node->getRight() );
            node->getRight()->setParent( node->getParent() );
            node->setParent( NULL );
        }
    }
}
//------------------------------------------------------------------------

void BinTree::printTree()
{
    if( isEmpty() )
        cout << "( empty tree )" << endl;
    else
    {
        cout << "Root: " << root->getValue() << endl;
        printTreeRec( root );
    }
}

void printTreeRec( Node *node )
{
    if( node->getLeft() != NULL )
        cout << "Left: " << node->getLeft()->getValue() << endl;
    if( node->getRight() != NULL )
        cout << "Right: " << node->getRight()->getValue() << endl;
    if( node->getLeft() != NULL )
        printTreeRec( node->getLeft() );
    if( node->getRight() != NULL )
        printTreeRec( node->getRight() );
}


