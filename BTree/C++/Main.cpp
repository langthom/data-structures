/* Copyright (c) 2015, Thomas Lang. All rights reserved.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */

#include <iostream>
#include "BTree.h"
using std::cout;
using std::endl;

/* Some testing ... */
int main(int argc, char* argv[])
{
    const int degree = 1;
    cout << "Creating a new BTree of degree " << degree << " ... ";
    BTree<int> tree(degree);
    cout << "done." << endl;

    for (int i = 0; i < 11; ++i) {
        tree.insert(i + 1);
    }

    cout << "--------------------------------------" << endl;
    cout << "The tree:" << endl;
    cout << tree << endl;

    cout << "Deleting '9' from tree ... ";
    bool delete9 = tree.remove(9);
    cout << "done, " << (delete9 ? "" : "not ") << "successful." << endl;
    cout << endl << "The tree now:" << endl;
    cout << tree << endl;

    cout << "Deleting '2' from tree ... ";
    bool delete2 = tree.remove(2);
    cout << "done, " << (delete2 ? "" : "not ") << "successful." << endl;
    cout << endl << "The tree now:" << endl;
    cout << tree << endl;

    cout << "Deleting '8' from tree ... ";
    bool delete8 = tree.remove(8);
    cout << "done, " << (delete8 ? "" : "not ") << "successful." << endl;
    cout << endl << "The tree now:" << endl;
    cout << tree << endl;
    
    cout << "Deleting '5' from tree ... ";
    bool delete5 = tree.remove(5);
    cout << "done, " << (delete5 ? "" : "not ") << "successful." << endl;
    cout << endl << "The tree now:" << endl;
    cout << tree << endl;

   cout << "Clearing tree ... ";
    tree.clear();
    cout << "done." << endl;
    return 0;
}
