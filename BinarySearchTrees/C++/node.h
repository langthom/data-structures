/* Node of a list.
 * (C) Thomas Lang, 2014
 *
 * This header contains a class representing a
 * single Node of a binary search tree.
 *
 * This code is licensed under the BSD3 license.
 */

#ifndef node_h___
#define node_h___

#include <iostream>
#include <cstdlib>

using std::cout;
using std::endl;


/* Class representing a single Node
 * in a binary search tree.
 *
 * @author Thomas Lang
 * @version July 16, 2014 
 */
class Node
{
    private:
        int value;
        Node *left;
        Node *right;
        Node *parent;
    public:
        /* Constructor for a new 
         * Node object, saves the
         * passed value in it.
         *
         * @param  val  The value to save.
         */
        Node( int val );

        /* Destructor for a Node.
         */
        ~Node(){}

        /* Getter for the saved value.
         *
         * @return The value saved in the Node.
         */
        int getValue()
        {
            return ( value );
        }

        /* Setter for the saved value.
         * Saves the parameter.
         *
         * @param  element  The new value to save.
         */
        void setValue( int element )
        {
            value = element;
        }

        /* Getter for "left"-reference.
         *
         * @return  The left pointer of the calling Node.
         */
        Node *getLeft()
        {
            return ( left );
        }

        /* Setter for the "left"-reference.
         *
         * @param  node  The new "left" pointer.
         */
        void setLeft( Node *node )
        {
            left = node;
        }

        /* Getter for the "right"-reference.
         *
         * @return The right pointer of the calling Node.
         */
        Node *getRight()
        {
            return ( right );
        }

        /* Setter for the "right"-reference
         *
         * @param  node  The new "right"-reference.
         */
        void setRight( Node *node )
        {
            right = node;
        }

        /* Getter for the "parent"-reference.
         *
         * @return The "parent" pointer of the calling Node.
         */
        Node *getParent()
        {
            return ( parent );
        }

        /* Setter for the "parent"-reference.
         *
         * @param  node  The new "parent"-reference.
         */
        void setParent( Node *node )
        {
            parent = node;
        }

        /* Prints a node to stdout.
         */
        void printNode()
        {
            cout << "{" << value << "}" << endl;
        }
};

#endif // node_h___
