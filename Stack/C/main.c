/* Testing module for Stack.
 * (C) Thomas Lang, 2014
 *
 * This module does some testing on the 
 * data structure Stack.
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>
#include "stack.h"

int main( int argc, char *argv[] )
{
    printf( "*** Initializing new Stack ... done.\n" );
    Stack *stack = newStack();

    printf( "*** Pushing values  7, 42, 24, and 999  to stack ... done.\n" );
    push( stack, 7   );
    push( stack, 42  );
    push( stack, 24  );
    push( stack, 999 );

    printf( "   ### Current stack:\n\n" );
    printStack( stack );

    printf( "   ### Top element: %d\n", top( stack )->value );
    printf( "*** Pop from stack ... done.\n" );
    pop( stack );

    printf( "   ### Current stack:\n\n" );
    printStack( stack );

    freeStack( stack );

    return 0;
}
