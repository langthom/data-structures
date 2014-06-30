// node.h - definitions for a binary search tree - node

#ifndef node_h___
#define node_h___

#include <iostream>
#include <cstdlib>

using std::cout;
using std::endl;


class Node
{
    private:
        int value;
        Node *left;
        Node *right;
        Node *parent;
    public:
        Node( int val );

        ~Node(){}

        int getValue()
        {
            return ( value );
        }

        void setValue( int element )
        {
            value = element;
        }

        Node *getLeft()
        {
            return ( left );
        }

        void setLeft( Node *node )
        {
            left = node;
        }

        Node *getRight()
        {
            return ( right );
        }

        void setRight( Node *node )
        {
            right = node;
        }

        Node *getParent()
        {
            return ( parent );
        }

        void setParent( Node *node )
        {
            parent = node;
        }

        void printNode()
        {
            cout << "{" << value << "}" << endl;
        }
};

#endif // node_h___
