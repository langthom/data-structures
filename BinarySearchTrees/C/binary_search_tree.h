// binary_search_tree.h - header for binary integer search tree

#ifndef binary_search_tree_h___
#define binary_search_tree_h___


//----------------------------------------------------------------

struct _Node_
{
    int value;
    struct _Node_ *left;
    struct _Node_ *right;
};

typedef struct _Node_ Node;

struct _BinTree_
{
    Node *root;
};

typedef struct _BinTree_ BinTree;

//-----------------------------------------------------------------

Node *newNode(  int element                );
int isEmpty(    BinTree *tree              );
void insert(    BinTree *tree, int element );
void printTree( BinTree *tree              ); 
void freeTree(  BinTree *tree              );


// helper functions, do not use them in your implementation
void insertIntoTree(    Node *node, int element );
void printTreeFromRoot( Node *node              );
void freeTreeRec(       Node *node              );


#endif // binary_search_tree_h___
