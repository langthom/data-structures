/* Node of a Heap.
 * (C) Thomas Lang, 2014
 *
 * This header implements a generic Node of
 * a heap. A Node saves a value and references
 * to its children and parent Node (if existant).
 *
 * This code is licensed under the BSD3 license.
 */

#ifndef node_h___
#define node_h___

#include <iostream>

using namespace std;

template <typename T> class Node
{
    private:
        T value;
        Node<T> *left;
        Node<T> *right;
        Node<T> *parent;

    public:
        /* Constructor for a new Node.
         * This one initializes the value
         * of the Node to the passed
         * parameter and all references
         * to NULL.
         *
         * @param  value_  The value to save.
         */
        Node( T value_ ):
            value( value_ ),
            left( NULL ),
            right( NULL ),
            parent( NULL )
            {}

        /* Destructor for a single Node.
         */
        ~Node(){}

        /* Receives the value of 
         * the calling Node.
         *
         * @return  The saved value.
         */
        T getValue()
        {
            return value;
        }

        /* Receives a pointer to
         * the left child of the
         * calling Node.
         *
         * @return  The left reference.
         */
        Node<T> *getLeft()
        {
            return left;
        }

        /* Receives a pointer to
         * the right child of the
         * calling Node.
         *
         * @return  The right reference.
         */
        Node<T> *getRight()
        {
            return right;
        }

        /* Receives a pointer to
         * the parent Node of the
         * calling Node.
         *
         * @return  The parent reference.
         */
        Node<T> *getParent()
        {
            return parent;
        }

        /* Sets the value of the Node
         * to the passed parameter.
         *
         * @param  value_  The new value to save.
         */
        void setValue( T value_ )
        {
            value = value_;
        }

        /* Sets the left reference of the
         * Node to the passed parameter.
         *
         * @param  node  The new left reference.
         */
        void setLeft( Node<T> *node )
        {
            left = node;
        }

        /* Sets the right reference of the
         * Node to the passed parameter.
         *
         * @param  node  The new right reference.
         */
        void setRight( Node<T> *node )
        {
            right = node;
        }

        /* Sets the parent reference of the
         * Node to the passed parameter.
         *
         * @param  node  The new parent reference.
         */
        void setParent( Node<T> *node )
        {
            parent = node;
        }

        /* Manipulates the output stream to get
         * a nice representation of a Node.
         *
         * @param  out  The output stream to manipulate
         * @param  n    A node.
         * @return A nice representation of a Node.
         */
        friend ostream& operator<<( ostream& out, Node<T> *n )
        {
            out << "{" << n->getValue() << "}";
            return out;
        }
};

#endif // node.h
