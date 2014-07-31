/* Double linked list header.
 * (C) Thomas Lang, 2014
 *
 * This header stores an implementation of a typical 
 * double linked list. This means there are two classes:
 *      * Node
 *      * List
 * 
 * This code is licensed under the BSD3 license.
 */

#ifndef double_linked_list_h___
#define double_linked_list_h___

#include <sstream>

using namespace std;

/* This class represents a single Node in the list.
 * A Node stores a value and a reference to its 
 * successor.
 *
 * @author Thomas Lang, 2014
 * @version July 15, 2014
 */
template <typename T>
class Node
{
    private:
        T value;
        Node<T> *next;
        Node<T> *prev;
    public:
        /* Constructor for a new Node,
         * stores the passed value and
         * initializes the "next" pointer
         * to NULL.
         *
         * @param  value  The value to store.
         */
        Node( T value ):
            value( value ),
            next( NULL ),
            prev( NULL )
            {}

        /* Destructor for a Node.
         */
        ~Node(){}

        /* Receives the value of 
         * the calling Node.
         *
         * @return The value of the Node.
         */
        T getValue()
        {
            return value;
        }

        /* Sets the value of the calling
         * Node to the passed parameter.
         *
         * @param  value  The new value to store.
         */
        void setValue( T value )
        {
            this->value = value;
        }

        /* Receives the reference to the
         * Next node.
         *
         * @return Pointer to the next node.
         */
        Node<T>* getNext()
        {
            return next;
        }

        /* Sets the next-reference to the
         * passed parameter.
         *
         * @param  node  The new "next"-Node.
         */
        void setNext( Node<T> *node )
        { 
            this->next = node;
        }

        /* Receives the reference to the
         * Previous node.
         *
         * @return Pointer to the previous node.
         */
        Node<T> *getPrev()
        {
            return prev;
        }

        /* Sets the previous node.
         *
         * @param  node  The new previous node.
         */
        void setPrev( Node<T> *node )
        {
            this->prev = node;
        }

        /* Overloading the << operator to give
         * a Node an appropiate String 
         * representation, just like the
         * .toString() function in Java.
         *
         * @param   out  The output-stream to write to
         * @param   n    The node to print.
         * @return  An appropiate String representation
         */
        friend ostream& operator<<( ostream& out, Node<T> n )
        {
            out << "{" << n.getValue() << "}";
            return out;
        }
};

/* This class represents a single linked list,
 * containing a variable number of Nodes.
 *
 * @author Thomas Lang, 2014
 * @version July 15, 2014
 */
template <typename T>
class List
{
    private:
        Node<T> *front;
        Node<T> *rear;
        int size;
    public:
        /* Constructor for a new list,
         * initializes the front and 
         * the rear pointer to NULL
         * and the size to 0.
         */
        List():
            front( NULL ),
            rear(  NULL ),
            size(  0    )
            {}

        /* Destructor for a List object.
         * If called, it destroys first 
         * all Nodes contained in the
         * List and then the List object
         * itself will be deleted.
         */
        ~List()
        {
            if( !isEmpty() )
            {
                Node<T> *tmp;
                while( !isEmpty() )
                {
                    tmp = getFront();
                    setFront( getFront()->getNext() );
                    delete tmp;
                }
            }
        }

        /* Checks, whether a List is
         * empty or not.
         *
         * @return True, if the list is empty,
         *         False otherwise
         */
        bool isEmpty()
        {
            return (front == NULL);
        }

        /* Receives the front pointer.
         *
         * @return Front-pointer
         */
        Node<T> *getFront()
        {
            return front;
        }

        /* Sets the front pointer to
         * the passed parameter.
         *
         * @param  node  The new front Node.
         */
        void setFront( Node<T> *node )
        {
            front = node;
        }
        
        /* Receives the rear pointer.
         *
         * @return Rear-pointer
         */
        Node<T> *getRear()
        {
            return rear;
        }

        /* Sets the rear pointer to
         * the passed parameter.
         *
         * @param  node  The new rear Node.
         */
        void setRear( Node<T> *node )
        {
            rear = node;
        }

        /* Receives the Size of the list,
         * that means the number of Nodes in it.
         *
         * @return The size of the list.
         */
        int getSize()
        {
            return size;
        }

        /* Adds a new Node containing the passed
         * parameter as value to the List.
         *
         * @param  value  The new value to store.
         */
        void add( T value )
        {
            Node<T> *node = new Node<T>( value );

            if( isEmpty() )
                setFront( node );
            else
            {
                getRear()->setNext( node );
                node->setPrev( getRear() );
            }
            setRear( node );
            size++;
        }

        /* Adds a new Node containing <code>value</code>
         * to the list on the front.
         *
         * @param  value  The value of the new Node.
         */
        void addFront( T value )
        {
            if( isEmpty() )
                add( value );
            else
            {
                Node<T> *node = new Node<T>( value );
                getFront()->setPrev( node );
                node->setNext( front );
                setFront( node );
                size++;
            }
        }

        /* Deletes and returns the last element of the list.
         *
         * @return The last element of the list.
         */
        Node<T> *pop()
        {
            Node<T> *last = getRear();
            setRear( getRear()->getPrev() );
            getRear()->setNext( NULL );
            size--;
            return last;
        }

        /* Overloading the << operator to give a list
         * a nice String representation.
         *
         * @param  out  The output-stream to write to.
         * @param  l    The list to print.
         * @return An appropiate String representation of a list.
         */
        friend ostream& operator<<( ostream& out, List<T> l )
        {
            out << "[";
            Node<T> *f = l.getFront();
            while( f != NULL )
            {
                out << *f << " ";
                f = f->getNext();
            }
            out << "]\n";
            return out;
        }
};

#endif

