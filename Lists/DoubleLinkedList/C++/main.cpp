/* Testing module for a double linked list.
 * (C) Thomas Lang, 2014
 *
 * This module performst the basic and typical operations
 * on a double linked list of Integers.
 *
 * This code is licensed under the BSD3 license.
 */

#include <iostream>
#include <cstdlib>

#include "double_linked_list.h"

using std::cout;
using std::endl;

int main()
{
    int i;
    List<int> *test = new List<int>();

    for( i = 0; i < 20; i++ )
        test->add( rand() % 100 );

    cout << "*** Creating new double linked list ... done." << endl;
    cout << "*** Filling in 20 random values ... done." << endl;
    cout << "\n   ### Current List: \n" << endl;
    cout << test << endl;

    cout << "\nRemoving two elements at the end ... done." << endl;

    test->pop(); test->pop();

    cout << "\n   ### Current List: \n" << endl;
    cout << "*** Adding value \"999\" ... done." << endl;

    test->add( 999 );

    cout << "\n   ### Current List: \n" << endl;
    cout << test << endl;

    return 0;
}
