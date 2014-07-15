/* Testing module for list.
 * (C) Thomas Lang, 2014
 *
 * This module does some small testing on a single linked list in C++.
 *
 * This code is licensed under the BSD3 license.
 */

#include <iostream>

#include "single_linked_list.h"

using std::cout;
using std::endl;

int main( )
{
    cout << "*** Initializing new list ... done." << endl;
    cout << "*** Filling in values: 2, 7, 42, 12, 0, (-5), 999 ... done." << endl;

    List<int> test;
    test.add( 2  ); test.add( 7 ); test.add( 42 );
    test.add( 12 ); test.add( 0 ); test.add( -5 );
    test.add( 999 );

    cout << "   ### Current list: " << endl;
    cout << test << endl;

    return (0);
}

