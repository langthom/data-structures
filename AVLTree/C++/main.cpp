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

#include "avl.h"

using std::cout;
using std::endl;

/*
 * Main testing function.
 *
 * @param argc
 *         Count of command line arguments.
 * @param argv
 *         Command line arguments (not used here).
 * @return Returns 0 on successful execution.
 */
int main(int argc, char *argv[]) {
    cout << "Creating tree ... ";
    AVLTree<int> *tree = new AVLTree<int>();
    cout << "done." << endl;

    cout << "Provocating rotation ... ";
    tree->insert(-42);
    tree->insert(7);
    tree->insert(999);
    cout << "done." << endl;
    cout << "Actual tree:\n" << endl;
    tree->print();

    cout << endl << "Provocating second rotation ... ";
    tree->insert(10);
    tree->insert(144);
    cout << "done." << endl;
    cout << "Actual tree:" << endl << endl;
    tree->print();

    cout << endl << "Provocating third rotation and root changing ... ";
    tree->insert(9);
    cout << "done." << endl;
    cout << "Actual tree:" << endl << endl;
    tree->print();

    cout << endl << "Does the tree contain '999'? -> ";
    const char *p = tree->contains(999) ? "true" : "false";
    cout << p << endl;
    cout << endl;

    cout << "Deleting root ... ";
    tree->remove(10);
    cout << "done." << endl << "Deleting nodes '42', '7' ... ";
    tree->remove(-42);
    tree->remove(7);
    cout << "done." << endl << endl;

    cout << "Does the tree contain '-42'? -> ";
    p = tree->contains(-42) ? "true" : "false";
    cout << p << endl << endl;

    cout << "Actual tree:" << endl << endl;
    tree->print();
    cout << endl << endl << "Clearing tree ... ";
    tree->clear();
    cout << "done." << endl;

    cout << "Tree should be empty now: ";
    p = tree->isEmpty() ? "true" : "false";
    cout << p << endl;
    return 0;
}
