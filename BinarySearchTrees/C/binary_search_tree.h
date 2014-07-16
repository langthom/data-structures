/* Binary search tree.
 * (C) Thomas Lang, 2014
 *
 * This header contains structs representing
 * a Node of a tree and the tree itself.
 * Furthermore it contains several methods
 * and functions like adding a new element.
 *
 * This code is licensed under the BSD3 license.
 */

#ifndef binary_search_tree_h___
#define binary_search_tree_h___


//----------------------------------------------------------------
/* Representation of a single Node element.
 * A Node contains a value and references
 * to its left and right child.
 *
 * @author Thomas Lang
 * @version July 16, 2014
 */
struct _Node_
{
    int value;
    struct _Node_ *left;
    struct _Node_ *right;
};

typedef struct _Node_ Node;

/* Struct representing a binary search 
 * tree. A tree does initially only 
 * contain the single root Node. New
 * Nodes can be added via the "insert"
 * method.
 *
 * @author Thomas Lang
 * @version July 16, 2014
 */
struct _BinTree_
{
    Node *root;
};

typedef struct _BinTree_ BinTree;


//-----------------------------------------------------------------
/* Constructor-like function.
 * This one allocates the needed memory and
 * initializes the value of the Node to the
 * parameter and the left and right pointers
 * to NULL.
 *
 * @param   element  The value to save.
 * @return  A pointer to the newly created Node.
 */
Node *newNode(  int element                );


/* Checks, whether a tree is empty or not.
 *
 * @param   tree  The tree to check
 * @return  -1 if the tree is empty, 
 *          0 otherwise
 */
int isEmpty(    BinTree *tree              );


/* Inserts a new Node containing the parameter
 * into the tree by traversing until the right
 * position is found.
 *
 * @param   tree    The tree to insert to.
 * @param   element The value to store in the new Node.
 */
void insert(    BinTree *tree, int element );


/* Prints a tree to stdout.
 *
 * @param  tre  The tree to print.
 */
void printTree( BinTree *tree              ); 


/* Frees the tree.
 * Gives memory free for overwriting and goes
 * postorder for first freeing all Nodes and
 * then the tree itself.
 *
 * @param  tree  The tree to free.
 */
void freeTree(  BinTree *tree              );


// helper functions, do not use them in your implementation
void insertIntoTree(    Node *node, int element );
void printTreeFromRoot( Node *node              );
void freeTreeRec(       Node *node              );


#endif // binary_search_tree_h___
