/* Testing module for a Queue.
 * (C) Thomas Lang, 2014
 *
 * This module performs some basic operations
 * on a Queue to show its functionality.
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "queue.h"

int main( int argc, char *argv[] )
{
    printf( "*** Initializing new Queue ... done.\n" );
    Queue *q = newQueue();

    printf( "*** Enqueue-ing 20 random values ... done.\n" );
    int i;
    srand( time( NULL ));
    for( i = 0; i < 20; i++ )
        enqueue( q, rand()%100 );

    printf( "\n   ### Current Queue: \n" );
    printf( "\t" );
    print( q );

    printf( "\n*** Enqueue-ing value 999 ... done.\n" );
    enqueue( q, 999 );

    printf( "\n   ### Current Queue: \n" );
    printf( "\t" );
    print( q );

    printf( "\n*** Dequeue-ing two times ... done.\n" );
    dequeue( q ); dequeue( q );

    printf( "\n   ### Current Queue: \n" );
    printf( "\t" );
    print( q );
    printf( "\n" );

    freeQueue( q );

    return 0;
}
