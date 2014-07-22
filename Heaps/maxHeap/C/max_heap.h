/* MaxHeap
 * (C) Thomas Lang, 2014
 *
 * This header implements a MaxHeap.
 * A MaxHeap is generally a binary tree that 
 * follows some rules:
 *      * The value of a node is ALWAYS 
 *        bigger than the value(s) of
 *        its child(ren).
 *      * It is not defined if the left
 *        child of a node is smaller or
 *        greater than the right child.
 * This gives the unique property, that the
 * root of such a tree is ALWAYS the global 
 * maximum of the whole tree. This fact helps
 * for efficient implementing many algorithms,
 * especially graph algorithms, of which the
 * most used function is "deleteMax".
 *
 * This code is licensed under the BSD3 license.
 * You are free to use, copy and distribute this code.
 */

#ifndef max_heap_h___
#define max_heap_h___


/* Structure representing a single Node element.
 * A Node saves a value (here an Integer) and
 * pointers to its left and right child and to
 * its parent Node.
 */
typedef struct __Node__
{
    int value;
    struct __Node__ *left;
    struct __Node__ *right;
    struct __Node__ *parent;
} Node;


/* Structure representing a MaxHeap.
 * A MaxHeap has a root, as it is a tree,
 * the size (the number of elements).
 */
typedef struct __MaxHeap__
{
    Node *root;
    int size;
} MaxHeap;


//-------------- Node-specific functions/methods --------------

/* Creates a new Node by allocating the needed memory.
 * Furthermore it saves the passed value and 
 * initializes the Node-references to NULL.
 *
 * @param  value  The value to save.
 * @return A pointer to the newly created Node.
 */
Node *newNode( int value );


/* Returns the valued saved in the Node.
 *
 * @param  node  The node to call.
 * @return The saved value of the passed node.
 */
int getValue( Node *node );


/* Sets the value of the Node.
 *
 * @param  node   The node to change the value.
 * @param  value  The new value to save.
 */
void setValue( Node *node, int value );


/* Returns the left pointer.
 *
 * @param  node  The node to call.
 * @return The left reference of the passed node.
 */
Node *getLeft( Node *node );


/* Sets the left reference of the passed Node.
 *
 * @param  node  The node to change its pointer.
 * @param  left  The new left reference
 */
void setLeft( Node *node, Node *left );


/* Returns the right pointer.
 *
 * @param  node  The node to call.
 * @return The right reference of the passed node.
 */
Node *getRight( Node *node );


/* Sets the right reference of the passed Node.
 *
 * @param  node   The node to change its pointer.
 * @param  right  The new right reference.
 */
void setRight( Node *node, Node *right );


/* Returns the parent pointer.
 *
 * @param  node  The node to call.
 * @return The parent reference of the passed node.
 */
Node *getParent( Node *node );


/* Sets the parent reference of the passed Node.
 *
 * @param  node    The node to call.
 * @param  parent  The new parent reference.
 */
void setParent( Node *node, Node *parent );



//----------- MaxHeap-specific functions/methods --------------


/* Creates a new MaxHeap by allocating the 
 * needed memory and initializing the attributes.
 *
 * @return A pointer to the newly created Heap.
 */
MaxHeap *newMaxHeap();


/* Checks if the Heap is empty.
 *
 * @return 0 if the heap is empty, -1 otherwise
 */
int isEmpty( MaxHeap *heap );


/* Returns the size of the tree.
 *
 * @return The number of elements saved in the heap.
 */
int getSize( MaxHeap *heap );


/* Returns the root of the passed heap.
 *
 * @param  heap  The heap to call.
 * @return A pointer to the root of the heap.
 */
Node *getRoot( MaxHeap *heap );


/* Inserts a new value to the heap and 
 * performs - if necessary - a "bubble-up".
 *
 * @param  heap   The heap to insert to.
 * @param  value  The new value to insert.
 */
void insert( MaxHeap *heap, int value );


/* Returns the last element added.
 *
 * @return A pointer to the last element added.
 */
Node *getLast();


/* Deletes the maximum of the heap (the root).
 *
 * @param  heap  The heap to call.
 */
void deleteMax( MaxHeap *heap );


/* Prints a passed heap to stdout.
 *
 * @param  heap  The heap to print.
 */
void print( MaxHeap *heap );


/* Frees the entire heap in postorder direction.
 *
 * @param  heap  The heap to free.
 */
void freeHeap( MaxHeap *heap );

#endif // max_heap.h
