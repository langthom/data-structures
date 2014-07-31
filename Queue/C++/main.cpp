/* Testing module for a Queue.
 * (C) Thomas Lang, 2014
 *
 * This module performst the basic and typical operations
 * on a Queue of Integers.
 *
 * This code is licensed under the BSD3 license.
 */

#include <iostream>
#include <cstdlib>

#include "queue.h"

using std::cout;
using std::endl;

int main()
{
    int i;
    Queue<int> *test = new Queue<int>();

    for( i = 0; i < 20; i++ )
        test->enqueue( rand() % 100 );

    cout << "*** Creating new Queue ... done." << endl;
    cout << "*** Filling in 20 random values ... done." << endl;
    cout << "\n   ### Current Queue: \n" << endl;
    cout << test << endl;

    cout << "\nRemoving two elements ... done." << endl;

    test->dequeue(); test->dequeue();

    cout << "\n   ### Current Queue: \n" << endl;
    cout << "*** Adding value \"999\" ... done." << endl;

    test->enqueue( 999 );

    cout << "\n   ### Current Queue: \n" << endl;
    cout << test << endl;

    return 0;
}
