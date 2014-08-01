/* DoubleLinkedList.c
 * (C) 2014, Thomas Lang
 *
 * This module implements all the things listes up
 * in the header "double_linked_list.h"
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>
#include <stdlib.h>

#include "double_linked_list.h"

//---------------------------------------------------------------------

List* newList()
{
    List* list = malloc( sizeof( List ));
    list->root = NULL;
    list->rear = NULL;
    list->size = 0;
    return (list);
}


//---------------------------------------------------------------------

Node* newNode( int val )
{
    Node *node  = malloc( sizeof( Node ));
    node->value = val;
    node->next  = NULL;
    node->prev  = NULL;
    return (node);
}

//---------------------------------------------------------------------

int isEmpty( List *list )
{
    if( list->root == NULL )
        return 0;
    return (-1);
}

//---------------------------------------------------------------------

void add( List *list, int value )
{
    Node *node = newNode( value );
    if( isEmpty( list ) == 0 )
    {
        list->root = node;
        list->rear = node;
        list->size++;
    }
    else
    {
        list->rear->next = node;
        node->prev       = list->rear;
        list->rear       = node;
        list->size++;
    }
}

//---------------------------------------------------------------------

void addFront( List *list, int value )
{
    if( isEmpty( list ) == 0 )
        add( list, value );
    else
    {
        Node *node       = newNode( value );
        node->next       = list->root;
        list->root->prev = node;
        list->root       = node;
        list->size++;
    }
}


//---------------------------------------------------------------------

void print( List *list )
{
    if( isEmpty( list ) == 0)
        printf( "(empty list)" );
    else
    {
        Node *tmp = list->root;
        while( tmp != NULL )
        {
            printf( "%d ", tmp->value );
            tmp = tmp->next;
        }
        printf( "\n" );
    }
}


//-------------------------------------------------------------------

void freeList( List *list )
{
    if( isEmpty( list ) == (-1) )
    {
        Node *tmp;
        while( isEmpty( list ) == -1 )
        {
            tmp = list->root;
            list->root = list->root->next;
            free( tmp );
        }
    }
    free( list );
}
