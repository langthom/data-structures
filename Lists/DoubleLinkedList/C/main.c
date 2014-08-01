/* Testing module.
 * (C) 2014, Thomas Lang
 *
 * This module does some testing on a double-linked lists.
 * It will add some values, prints the list and lastly
 * frees the entire list.
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>
#include <stdlib.h>

#include "double_linked_list.h"

int main( int argc, char *argv[] )
{
    printf( "*** Initializing new list ... done. \n" );
    List *test = newList();
    printf( "*** Adding values: 2, 42, 8, 7, 999 ... done.\n" );
    add( test, 2   );
    add( test, 42  );
    add( test, 8   );
    add( test, 7   );
    add( test, 999 );

    printf( "\n   ### Current list: \n" );
    print( test );
    printf( "\n" );

    printf( "*** Adding value  212  to the front ... done.\n " );
    addFront( test, 212 );

    printf( "\n   ### Current list: \n" );
    print( test );
    printf( "\n" );

    freeList( test );

    return (0);
}
