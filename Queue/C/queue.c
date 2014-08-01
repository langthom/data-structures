/* Implementation of function in "queue.h"
 * (C) Thomas Lang, 2014
 *
 * This module implements all functions 
 * that are declared in the header "queue.h".
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdlib.h>

#include "queue.h"

//-----------------------------------------------

Queue *newQueue()
{
    Queue *queue = malloc( sizeof( Queue ));
    queue->list  = newList();
    return queue;
}


//-----------------------------------------------

int isEmpty( Queue *queue )
{
    return lIsEmpty( queue->list );
}


//-----------------------------------------------

void enqueue( Queue *queue, int element )
{
    addFront( queue->list, element );
}


//-----------------------------------------------

Node *dequeue( Queue *queue )
{
    return pop( queue->list );
}


//-----------------------------------------------

void print( Queue *queue )
{
    lPrint( queue->list );
}


//-----------------------------------------------

void freeQueue( Queue *queue )
{
    freeList( queue->list );
    free( queue );
}
