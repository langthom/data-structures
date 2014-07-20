/* Testing module for bubble sort.
 * (C) Thomas Lang, 2014
 *
 * This module tests the bubble-sort
 * algorithm on a single array.
 *
 * This code is licensed under the BSD3 license.
 */

#include <iostream>
#include "bubble_sort.h"

#define SIZE 10

using std::cout;
using std::endl;

void printArray( int *array )
{
    int i;
    cout << "[ ";
    for( i = 0; i < SIZE; i++ )
        cout << array[i] << " ";
    cout << "]\n" << endl;
}

int main( int argc, char *argv[] )
{
    cout << "*** Initializing array-to-sort ... done.\n" << endl;
    int arr[SIZE] = { 42, 0, 58, 723, (-13), 7, 999, 8, 42, 21 };
    cout << "   ### Array to sort:\n" << endl;
    printArray( arr );

    cout << "\n   ### Size of the array: " << SIZE << endl << endl;
    cout << "*** Sorting array with bubble sort ... done.\n" << endl;
    bubble_sort( arr, SIZE );
    
    cout << "   ### Sorted array:\n" << endl;
    printArray( arr );

    return 0;
}

