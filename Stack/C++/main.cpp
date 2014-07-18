/* Testing module for Stack.
 * (C) Thomas Lang, 2014
 *
 * This module does some testing on a Stack.
 *
 * This code is licensed under the BSD3 license.
 */

#include <iostream>

#include "stack.h"


using std::cout;
using std::endl;

int main()
{
    cout << "*** Initializing new Stack ... done." << endl;
    Stack<int> stack;

    cout << "*** Filling in values  7, 42, 24, and 999 ... done." << endl;
    stack.push( 7   );
    stack.push( 42  );
    stack.push( 24  );
    stack.push( 999 );

    cout << "   ### Current stack:" << endl;
    stack.print();

    cout << "*** Performing Pop on stack ... done." << endl;
    stack.pop();

    cout << "   ### Current stack:" << endl;
    stack.print();

    return 0;
}
