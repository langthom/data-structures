/* Implementation of a MaxHeap.
 * (C) Thomas Lang, 2014
 *
 * This header implements a specialization of the 
 * abstract Heap, the MaxHeap.
 * This is just like the abstract one, but it follows
 * the rule that a node is always bigger than its 
 * children. Because of that, the root of this Heap
 * is always the global maximum of all elements and
 * so we provide the special method "deleteMax" which
 * is useful for many many algorithms.
 * 
 * This code is licensed under the BSD3 license.
 */

#ifndef max_heap_h___
#define max_heap_h___

#include "heap.h"

template <typename T> class MaxHeap : public Heap<T>
{
    private:
        /* Swaps two nodes (values only).
         *
         * @param  child  The first node to swap.
         * @param  parent The second node to swap.
         */
        void swapNodes( Node<T> *child, Node<T> *parent )
        {
            T childValue = child->getValue();
            child->setValue( parent->getValue() );
            parent->setValue( childValue );
        }

        /* Performs a "bubbleDown" operation, so if the
         * value of a node is here bigger than one/two 
         * of its children, the appropriate nodes are
         * swapped until everything is fine again.
         *
         * @param  node  The node to "bubble down".
         */
        void bubbleDown( Node<T> *node )
        {
            if( !node->getLeft() && !node->getRight() )
                return;

            if( node->getLeft() && node->getRight() )
            {
                if(( node->getValue() < node->getLeft()->getValue() ) &&
                   ( node->getLeft()->getValue() > node->getRight()->getValue() ))
                {
                    swapNodes( node, node->getLeft() );
                    bubbleDown( node->getLeft() );
                }
                else if( node->getValue() < node->getRight()->getValue() )
                {
                    swapNodes( node, node->getRight() );
                    bubbleDown( node->getRight() );
                }
            }

            if( node->getLeft() && (node->getValue() < node->getLeft()->getValue() ))
            {
                swapNodes( node, node->getLeft() );
                bubbleDown( node->getLeft() );
            }

            if( node->getRight() && (node->getValue() < node->getRight()->getValue() ))
            {
                swapNodes( node, node->getRight() );
                bubbleDown( node->getRight() );
            }
        }

        /* Performs a "bubbleUp" operation, so if a Node's value
         * is smaller than its parent, the two Nodes are swapped.
         *
         * @param  node  The node to "bubble Up".
         */
        void bubbleUp( Node<T> *node )
        {
            if( !node->getParent() )
                return;
            if( node->getValue() > node->getParent()->getValue() )
            {
                swapNodes( node, node->getParent() );
                bubbleUp( node->getParent() );
            }
        }

    public:
        /* Constructor for a new MaxHeap,
         * calls the Constructor of an
         * abstract Heap.
         */
        MaxHeap():Heap<T>(){}

        // destructor is automatically called
        // from superclass, doesn't need 
        // to be called explicitly

        /* Inserts a new Node containing the 
         * value <code>value</code> to the 
         * Heap by creating the Node, inserting
         * it and then performs a bubbleUp
         * (if necessary).
         *
         * @param  value  The value of the new Node
         *                to insert.
         */
        void insert( T value )
        {
            Node<T> *node = new Node<T>( value );
            Heap<T>::insert( node );
            bubbleUp( node );
        }

        /* Performs the deleteMax operation.
         * That means this method deletes the root of
         * the Heap by firstly swapping its value with
         * the one of the last Node added to the Heap 
         * and then deleting the last Node added
         * (which carries the value of the root now).
         * After that - if necessary - a bubbleDown
         * will be performed.
         */
        void deleteMax()
        {
            Node<T> *lastOne = Heap<T>::getLastNode();
            Heap<T>::getRoot()->setValue( lastOne->getValue() );

            if( lastOne->getParent()->getLeft() == lastOne )
            {
                lastOne->getParent()->setLeft( NULL );
                lastOne->setParent( NULL );
            }
            else
            {
                lastOne->getParent()->setRight( NULL );
                lastOne->setParent( NULL );
            }
            Heap<T>::decreaseSize();

            bubbleDown( Heap<T>::getRoot() );
        }

        /* Prints a MaxHeap to stdout.
         */
        void print()
        {
            Heap<T>::print();
        }
};

#endif // min_heap.h
