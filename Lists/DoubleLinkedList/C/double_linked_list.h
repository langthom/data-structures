/* List header.
 * (C) 2014, Thomas Lang
 *
 * This header contains a datatype for a double linked list
 * that contains "Nodes", that itself contains Integer values.
 * It provides methods and functions for acting on such lists.
 *
 * This code is licensed under the BSD3 license.
 */

#ifndef double_linked_list_h___
#define double_linked_list_h___

struct __Node__
{
    int value;
    struct __Node__ *next;
    struct __Node__ *prev;
};

typedef struct __Node__ Node;

struct __List__
{
    Node *root;
    Node *rear;
    int size;
};

typedef struct __List__ List;


//---------------------------------------------------------
/* Creates an empty double linked list. 
 * For that it initializes the root and rear pointer to
 * NULL and the size to 0.
 *
 * @return a pointer to the newly created list.
 */
List* newList();


//---------------------------------------------------------
/* Creates a new Node-element containing the value "val".
 * Initializes the "next"-pointer to NULL.
 * 
 * @param  val  the integer value to save in the Node.
 * @return a pointer to the newly created Node.
 */
Node* newNode( int val );


//---------------------------------------------------------
/* Checks whether a list is empty or not.
 *
 * @param  list  the list to check.
 * @return 0 if the list is empty, -1 if not.
 */
int isEmpty( List *list );


//---------------------------------------------------------
/* Appends a new Node containing the value "value" to
 * the passed List "list".
 *
 * @param  list   the list to append to
 * @param  value  the value to append 
 */
void add( List *list, int value );


//---------------------------------------------------------
/* Adds a new Node containing the value "value" to
 * the passed List "list" at the front.
 *
 * @param  list   The list to add to
 * @param  value  The value to add.
 */
void addFront( List *list, int value );


//---------------------------------------------------------
/* Prints the passed List to stdout.
 *
 * @param  list  The list to print.
 */
void print(  List *list );


//---------------------------------------------------------
/* Frees all Nodes from the passed list and finally
 * also the list itself.
 *
 * @param  list  The list to free.
 */
void freeList( List *list );

#endif  // double_linked_list_h___
