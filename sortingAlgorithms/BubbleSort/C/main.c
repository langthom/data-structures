/* Testing module for bubble sort.
 * (C) Thomas Lang, 2014
 *
 * This module tests the bubble-sort
 * algorithm on a single array.
 *
 * This code is licensed under the BSD3 license.
 */

#include <stdio.h>
#include "bubble_sort.h"

#define SIZE 10

void printArray( int *array )
{
    int i;
    printf( "[ " );
    for( i = 0; i < SIZE; i++ )
        printf( "%d ", array[i] );
    printf( "]\n" );
}

int main( int argc, char *argv[] )
{
    printf( "*** Initializing array-to-sort ... done.\n" );
    int arr[SIZE] = { 42, 0, 58, 723, (-13), 7, 999, 8, 42, 21 };
    printf( "   ### Array to sort:\n" );
    printArray( arr );

    printf( "\n   ### Size of the array: %d\n\n", SIZE );
    printf( "*** Sorting array with bubble sort ... done.\n" );
    bubble_sort( arr, SIZE );
    
    printf( "   ### Sorted array:\n" );
    printArray( arr );

    return 0;
}

