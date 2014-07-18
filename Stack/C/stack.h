/* Stack
 * (C) Thomas Lang, 2014
 *
 * This header implements a typical Stack.
 * A Stack is a data structure that allows
 * access only to the top element, the 
 * stack pointer. 
 *
 * This code is licensed under the BSD3 license.
 */

#ifndef stack_h___
#define stack_h___

/* Struct of a Node.
 * A Node contains a value (here of type
 * integer), and a reference to the element
 * that lies directly below the current one.
 */
typedef struct __Node__
{
    int value;
    struct __Node__ *next;
} Node;


/* Struct of a Stack.
 * A Stack initially contains a reference 
 * to the most-top element stored, the so-
 * called "stack pointer", and the size.
 */
typedef struct __Stack__
{
    Node *sp; // sp = stack-pointer
    int size;
} Stack;

//--------------------------------------------------
/* Creates a new Node.
 * This function creates a new Node by allocating
 * the needed memory and then initializing the
 * attribute "value" to the passed parameter and
 * the attribute "next" to NULL.
 *
 * @param  value  The value to store.
 * @return A pointer to the newly created Node.
 */
Node *newNode( int value );


//---------------------------------------------------
/* Checks, whether the passed stack is empty or not.
 *
 * @param  stack  The stack to check.
 * @return 0 if the stack is empty, (-1) otherwise
 */
int isEmpty( Stack *stack );


//---------------------------------------------------
/* Creates a new Stack.
 * This function creates a new Stack by allocating
 * the needed memory and then initializing the 
 * attribute "sp" to NULL and the size to 0.
 *
 * @return A pointer to the newly created Stack.
 */
Stack *newStack();


//---------------------------------------------------
/* Returns the size of the passed stack.
 *
 * @param  stack  The stack containing the wished size.
 * @return The size of the passed stack.
 */
int getSize( Stack *stack );


//----------------------------------------------------
/* Pushes a new Node containing the value "value" to
 * the passed stack.
 *
 * @param  stack  The stack to push to.
 * @param  value  The new value to store.
 */
void push( Stack *stack, int value );


//------------------------------------------------------
/* Deletes the top element of the stack and removes it.
 *
 * @param  stack  The stack to operate on.
 * @return The deleted Node.
 */
Node *pop( Stack *stack );


//-----------------------------------------------------
/* Returns the top element of the stack without removing it.
 *
 * @param  stack  The stack to operate on.
 * @return The top element of the stack.
 */
Node *top( Stack *stack );


//-----------------------------------------------------------
/* Prints a stack to stdout.
 *
 * @param  stack  The stack to print.
 */
void printStack( Stack *stack );


#endif // stack.h
