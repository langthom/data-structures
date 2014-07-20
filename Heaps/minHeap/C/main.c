/* Testing module for MinHeap.
 * (C) Thomas Lang, 2014
 *
 * This module does some testing on a MinHeap.
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>

#include "min_heap.h"

int main( int argc, char *argv[] )
{
    printf( "*** Initializing new MinHeap ... done.\n" );
    printf( "*** Adding values 7, 42, 999, 0, 212, 512, 21 ... done.\n" );
    
    MinHeap *heap = newMinHeap();
    insert( heap, 7   );
    insert( heap, 42  );
    insert( heap, 999 );
    insert( heap, 0   );
    insert( heap, 212 );
    insert( heap, 512 );
    insert( heap, 21  );

    printf( "   ### Current MinHeap:\n" );
    print( heap );
    printf( "\n   ### Number of Nodes: %d\n", getSize( heap ));
    printf( "\n*** Performing \"deleteMin\" ... done.\n" );

    deleteMin( heap );

    printf( "   ### Current MinHeap:\n" );
    print( heap );
    printf( "\n   ### Number of Nodes: %d\n\n", getSize( heap ));

    freeHeap( heap );

    return 0;
}


