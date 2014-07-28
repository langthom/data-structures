/* Implementation of an abstract heap.
 * (C) Thomas Lang, 2014
 *
 * A heap is principally a binary tree which follows
 * the following rules:
 *      * A node is EVERYTIME bigger or smaller than
 *        its children. If its bigger or smaller 
 *        depends on the implementation.
 *      * There is absolutely NO DEPENDENCE between
 *        the left and the right child of a Node, that
 *        means that it is undefined, if the left child
 *        is smaller than the right or otherwise.
 * 
 * This code is licensed under the BSD3 license.
 */

#ifndef heap_h___
#define heap_h___

#include <iostream>
#include "node.h"

using std::cout;
using std::endl;

template <typename T>
class Heap
{
    private:
        Node<T> *last;  // reference to the last Node added

        /* Sets the "last" reference.
         *
         * @param  newLast  The new last reference.
         */
        void setLastNode( Node<T> *newLast )
        {
            last = newLast;
        }

        /* Calls the constructor for each Node 
         * running through in Postorder notation.
         *
         * @param  node  Current Node.
         */
        void freeHeapRec( Node<T> *node )
        {
            if( node->getLeft() )
                freeHeapRec( node->getLeft() );
            if( node->getRight() )
                freeHeapRec( node->getRight() );
            if( !node->getLeft() && !node->getRight() )
                delete[] node;
        }

        /* Inserts the Node <code>newNode</code> recursively
         * into the heap by comparing with <code>node</code>.
         *
         * @param  node     Current node.
         * @param  newNode  The new Node to insert.
         */
        void insert( Node<T> *node, Node<T> *newNode )
        {
            if( !node )
            {
                node = newNode;
                setLastNode( newNode );
            }
            else if( !node->getLeft() )
            {
                node->setLeft( newNode );
                newNode->setParent( node );
                setLastNode( newNode );
            }
            else if( !node->getRight() )
            {
                node->setRight( newNode );
                newNode->setParent( node );
                setLastNode( newNode );
            }
            else if( isFullTree( node ) && isFullTree( node->getLeft() ))
                insert( node->getRight(), newNode );
            else if( isFullTree( node ) )
                insert( node->getLeft(), newNode );
        }

        /* Helper function for checking, if the parameter
         * Node has both a left and right child.
         *
         * @param  node  The node to check.
         * @return {@code true} if node has both left and 
         *         right children, {@code false} otherwise.
         */
        bool isFullTree( Node<T> *node )
        {
            if( node && node->getLeft() && node->getRight() )
                return true;
            return false;
        }

        /* Helper function for printing the Heap.
         *
         * @param  node  Current node.
         */
        void printTreeRec( Node<T> *node )
        {
            if( node->getLeft() )
                cout << "Left: " << node->getLeft()->getValue() << " " << endl;
            if( node->getRight() )
                cout << "Right: " << node->getRight()->getValue() << endl;
            if( node->getLeft() )
                printTreeRec( node->getLeft() );
            if( node->getRight() )
                printTreeRec( node->getRight() );
        }


    protected:
        Node<T> *root;
        int size;

    public:
        /* Receives a reference to 
         * the last node added.
         *
         * @return Either {@code NULL} if the
         *         Heap is empty, or the reference
         *         to the last Node added.
         */
        Node<T> *getLastNode()
        {
            if( isEmpty() )
                return NULL;
            return last;
        }

        /* Constructor for creating a new
         * but empty Heap.
         */
        Heap():
            root( NULL ),
            size( 0 )
            {}

        /* Destructor for a Heap.
         * This is virtual because it
         * will be inheritated by the 
         * class "MinHeap".
         */
        virtual ~Heap()
        {
            if( isEmpty() )
                return;
            else
                freeHeapRec( root );
        }

        /* Returns a reference to the
         * root of the Heap.
         *
         * @return  The root-pointer.
         */
        Node<T> *getRoot()
        {
            return root;
        }

        /* Checks whether a heap 
         * is empty or not.
         *
         * @return {@code true} if the
         *         heap is empty,
         *         {@code false} otherwise.
         */
        bool isEmpty()
        {
            return (root == NULL);
        }

        /* Returns the size of the heap.
         *
         * @return The number of elements in the heap.
         */
        int getSize()
        {
            return size;
        }

        /* Decreases the size attribute 
         * if an element will be deleted.
         */
        void decreaseSize()
        {
            size--;
        }

        /* Inserts the Node <code>node</code>
         * into the heap.
         *
         * @param  node  The node to insert.
         */
        void insert( Node<T> *node )
        {
            if( isEmpty() )
            {
                root = node;
                setLastNode( node );
            }
            else
            {
                insert( root, node );
            }
            size++;
        }

        /* Prints a heap to stdout.
         */
        void print()
        {
            if( isEmpty() )
            {
                cout << "(empty tree)" << endl;
                return;
            }
            cout << "Root: " << root->getValue() << endl;
            printTreeRec( root );
        }
};


#endif // heap.h
