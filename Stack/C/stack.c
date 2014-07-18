/* Stack
 * (C) Thomas Lang, 2014
 *
 * This module implements functions and methods
 * used for working with a typical Stack data
 * structure, explanation can be found in the header.
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

//----------------------------------------
Node *newNode( int value )
{
    Node *node  = malloc( sizeof( Node ));
    node->value = value;
    node->next  = NULL;
    return node;
}

//---------------------------------------
int getSize( Stack *stack )
{
    return stack->size;
}


//---------------------------------------
Stack *newStack()
{
    Stack *stack = malloc( sizeof( Stack ));
    stack->sp    = NULL;
    stack->size  = 0;
    return stack;
}


//--------------------------------------
int isEmpty( Stack *stack )
{
    if( stack->sp == NULL )
        return 0;
    return (-1);
}

//-------------------------------------
void push( Stack *stack, int value )
{
    Node *node = newNode( value );
    if( isEmpty( stack ) == 0 )
    {
        stack->sp  = node;
    }
    else
    {
        node->next = stack->sp;
        stack->sp  = node;
    }
    stack->size++;
}

//------------------------------------
Node *pop( Stack *stack )
{
    if( isEmpty( stack ) == 0 )
    {
        printf( "Cannot pop from empty Stack!\n" );
        return NULL;
    }
    Node *tmp = stack->sp;
    stack->sp = stack->sp->next;
    stack->size--;
    return tmp;
}

//------------------------------------
Node *top( Stack *stack )
{
    return stack->sp;
}


//-------------------------------------------
void printStack( Stack *stack )
{
    if( isEmpty( stack ) == 0)
        printf( "(empty stack)\n" );
    else
    {
        printf( "---> %d\n", stack->sp->value );
        Node *top = stack->sp->next;
        while( top != NULL )
        {
            printf( "     %d\n", top->value );
            top = top->next;
        }
        printf( "\n" );
    }
}
