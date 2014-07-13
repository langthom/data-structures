/* Testing module.
 * (C) 2014, Thomas Lang
 *
 * This module does some testing on single-linked lists.
 * It will add some values, prints the list and lastly
 * frees the entire list.
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>
#include <stdlib.h>

#include "single_linked_list.h"

int main( int argc, char *argv[] )
{
    printf( "*** Initializing new list... \n" );
    List *test = newList();
    printf( "*** Adding values: 2,42,8,7,999\n" );
    add( test, 2   );
    add( test, 42  );
    add( test, 8   );
    add( test, 7   );
    add( test, 999 );

    printf( "*** Current list: \n" );
    print( test );
    printf( "\n" );

    freeList( test );

    return (0);
}
