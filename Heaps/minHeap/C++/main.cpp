/* Testing module for MinHeap.
 * (C) Thomas Lang, 2014
 *
 * This module does some testing on a MinHeap.
 *
 * This code is licensed under the BSD3 license.
 */

#include <iostream>

#include "min_heap.h"

using std::cout;
using std::endl;

int main( int argc, char *argv[] )
{
    cout << "*** Initializing new MinHeap ... done." << endl;
    cout << "*** Adding values 7, 42, 999, 0, 212, 512, 21 ... done." << endl;
    
    MinHeap<int> *heap = new MinHeap<int>();
    heap->insert(  7   );
    heap->insert(  42  );
    heap->insert(  999 );
    heap->insert(  0   );
    heap->insert(  212 );
    heap->insert(  512 );
    heap->insert(  21  );

    cout << "   ### Current MinHeap:" << endl;
    heap->print();
    cout << "\n   ### Number of Nodes: " << heap->getSize() << endl;
    cout << "\n*** Performing \"deleteMin\" ... done." << endl;

    heap->deleteMin();

    cout << "   ### Current MinHeap:" << endl;
    heap->print();
    cout << "\n   ### Number of Nodes: " << heap->getSize() << endl;

    return 0;
}


