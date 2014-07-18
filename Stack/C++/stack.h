/* Implementation of a Stack.
 * (C) Thomas Lang, 2014
 *
 * This header implements a generic Stack.
 * A Stack is a data structure that allows 
 * access only to the most top element, the
 * so-called stack pointer, abbreviated as "sp".
 *
 * This code is licensed under the BSD3 license.
 */

#ifndef stack_h___
#define stack_h___

#include <iostream>

using namespace std;


/* Implementation of a template Node.
 * A node contains a value and a 
 * pointer to its successor.
 */
template <typename T>
class Node
{
    private:
        T value;
        Node *next;
    public:
        /* Constructor for a new Node,
         * saves the passed value and
         * initializes the "next" 
         * pointer to NULL.
         *
         * @param  value_  The value to save.
         */
        Node( T value_ ):
            value( value_ ),
            next( NULL )
            {}


        /* Destructor for a Node object.
         */
        ~Node(){}


        /* Receives the value saved in a Node.
         *
         * @return The value of the calling Node.
         */
        T getValue()
        {
            return value;
        }


        /* Sets the passed value as the new saved one.
         *
         * @param  value  The new value to save.
         */
        void setValue( T value )
        {
            this.value = value;
        }


        /* Receives the successor of the calling Node.
         *
         * @return  The next Node following.
         */
        Node<T> *getNext()
        {
            return next;
        }


        /* Sets the next pointer to the passed Node.
         *
         * @param  node  The new "next" reference.
         */
        void setNext( Node<T> *node )
        {
            next = node;
        }

        /* Manipulates the ouputstream of a Node.
         */
        friend ostream& operator<<( ostream& out, Node<T> n )
        {
            out << "{" << n.getValue() << "}";
            return out;
        }
};


/* Implementation of the Stack.
 * The Stack saves the size of the Stack and
 * the most top Node, the stackpointer.
 */
template <typename T>
class Stack
{
    private:
        Node<T> *sp;
        int size;
    public:
        /* Constructor for a new, empty Stack.
         */
        Stack():
            sp( NULL ),
            size( 0 )
            {}


        /* Destructor for a Stack.
         * It deletes all the Nodes first and
         * after that, it deletes the stack itself.
         */
        ~Stack()
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


        /* Checks whether the Stack is empty or not.
         *
         * @return true, if the stack is empty, 
         *         false otherwise
         */
        bool isEmpty()
        {
            return ( sp == NULL );
        }


        /* Receives the size of the stack.
         *
         * @return The size of the stack.
         */
        int getSize()
        {
            return size;
        }


        /* Receives the Stackpointer.
         *
         * @return A pointer to the stackpointer.
         */
        Node<T> *getFront()
        {
            return sp;
        }

        /* Sets the stackpointer to the passed Node.
         *
         * @param  node  The new stackpointer.
         */
        void setFront( Node<T> *node )
        {
            sp = node;
        }


        /* Pushes a new Node containing the passed
         * value to the Stack.
         *
         * @param  value  Value of the new Node to save.
         */
        void push( T value )
        {
            Node<T> *node = new Node<T>( value );

            if( isEmpty() )
            {
                sp = node;
            }
            else
            {
                node->setNext( sp );
                setFront( node );
            }
            size++;
        }

        
        /* Deletes and returns the most top element.
         *
         * @return  The stackpointer.
         */
        Node<T> *pop()
        {
            Node<T> *top = getFront();
            setFront( getFront()->getNext() );
            size--;
            return top;
        }

        
        /* Receives the stackpointer WITHOUT deleting it.
         *
         * @return The stackpointer.
         */
        Node<T> *top()
        {
            return getFront();
        }


        /* Prints a stack to stdout.
         */
        void print()
        {
            cout << "---> " << getFront()->getValue() << endl;
            Node<T> *top = getFront()->getNext();
            while( top != NULL )
            {
                cout << "     " << top->getValue() << endl;
                top = top->getNext();
            }
            cout << endl;
        }
};




#endif // stack.h
