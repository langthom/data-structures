/* Implementation of a Queue.
 * (C) Thomas Lang, 2014
 *
 * This header implements a generic Queue.
 * A Queue is basically a List that supports
 * the operations "enqueue" (adding an element),
 * and "dequeue" (removing an element), but
 * with the rule that if an element can be added
 * on the one end, removing an element MUST BE
 * performed on the exact other end.
 *
 * This code is licensed under the BSD3 license.
 */

#ifndef queue_h___
#define queue_h___

#include "double_linked_list.h"

template <typename T>
class Queue
{
    private:
        List<T> *list;

    public:
        /* Constructor, creates a new
         * Queue, so it initializes 
         * the representing list.
         */
        Queue()
        {
            list = new List<T>();
        }

        /* Checks whether the Queue is
         * empty or not.
         *
         * @return True if the Queue is empty,
         *         False otherwise.
         */
        bool isEmpty()
        {
            return list->isEmpty();
        }

        /* Adds a new Node containing the value
         * "element" to the Queue at the front.
         *
         * @param  element  The new element to add.
         */
        void enqueue( T element )
        {
            list->addFront( element );
        }

        /* Removes an element from the END of the Queue.
         *
         * @return The last element of the Queue.
         */
        Node<T> *dequeue()
        {
            return list->pop();
        }

        /* Overrides the output stream to get a 
         * nice representation of the Queue.
         *
         * @param  out  The stream to manipulate.
         * @param  q    The queue to print.
         * @return A nice representation of the Queue.
         */
        friend ostream& operator<<( ostream& out, Queue<T> *q )
        {
            out << "[";
            Node<T> *f = q->list->getFront();
            while( f != NULL )
            {
                out << *f << " ";
                f = f->getNext();
            }
            out << "]\n";
            return out;
        }
};

#endif // queue.h
