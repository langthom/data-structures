/* Header for a Queue of Integers.
 * (C) Thomas Lang, 2014
 *
 * This header provides functions for operating on
 * the data structure "Queue".
 * A Queue is basically a double linked list with
 * the operations enqueue (adding a new element)
 * and dequeue (removing an element).
 * This operations follow the rule that if the new
 * element is added (enqueue) at the FRONT, then
 * the dequeue operation takes place at THE OPPOSITE
 * site of the list.
 *
 * This code is licensed under the BSD3 license.
 */

#ifndef queue_h___
#define queue_h___

#include "double_linked_list.h"


typedef struct __Queue__
{
    List *list;
} Queue;


/* Creates a new but empty queue.
 *
 * @return A pointer to the newly created list.
 */
Queue *newQueue();


/* Checks whether the passed queue 
 * is empty or not.
 *
 * @param  queue  The queue to check.
 * @return 0 if the queue is empty,
 *         (-1) otherwise.
 */
int isEmpty( Queue *queue );


/* Adds a new Element at the FRONT of the queue.
 *
 * @param  queue    The queue to add to.
 * @param  element  The new value to add.
 */
void enqueue( Queue *queue, int element );


/* Removes the LAST ELEMENT of the queue.
 *
 * @param  queue  The queue to dequeue.
 * @return The last Node of the queue.
 */
Node *dequeue( Queue *queue );


/* Prints the queue to stdout.
 *
 * @param  queue  The queue to print.
 */
void print( Queue *queue );


/* Frees the entire Queue.
 *
 * @param  queue  The Queue to free.
 */
void freeQueue( Queue *queue );

#endif // queue.h
