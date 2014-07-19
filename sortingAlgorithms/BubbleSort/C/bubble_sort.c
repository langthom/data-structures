/* Implementation of Bubble sort.
 * (C) Thomas Lang, 2014
 *
 * Here the bubble sort algorithm is implemented.
 * The bubble sort algorithm runs over the passed
 * array and swaps two nodes if they are not sorted.
 * The main advantage of the bubble sort is that it
 * can handle already sorted lists.
 * This algorithm runs in  O(n^2).
 *
 * This code is licensed under the BSD3 license.
 */

#include "bubble_sort.h"

void bubble_sort( int array[], int size )
{
    int i, j, ready = 0;
    for( i = size; (i > 0) && (ready != 1); i-- )
    {
        ready = 1;
        for( j = 0; j < i - 1; j++ )
        {
            if( array[j] > array[j+1] )
            {
                int tmp    = array[j];
                array[j]   = array[j+1];
                array[j+1] = tmp;
                ready = 0;
            }
        }
    }
}

