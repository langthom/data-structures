// bintree.h - declarations for a binary search tree

#include "node.h"

class BinTree
{
    private:
        Node *root;
    public: 
        /* Default constructor for the tree,
         * initializes the root to NULL.
         */
        BinTree();

        /* Destructor, calls a helper method
         * to delete all nodes and then the
         * tree itself.
         */
        ~BinTree();

        /* Function that checks whether the
         * tree is empty or not.
         */
        bool isEmpty()
        {
            return ( root == NULL );
        }

        /* Inserts a new Node with content
         * element into the tree and searches
         * the appropiate position.
         *
         * @param  element  The element to insert.
         */
        void insert( int element );

        /* Traverses the tree in Preorder direction.
         */
        void traversePreorder();

        /* Traverses the tree in Inorder direction.
         */
        void traverseInorder();

        /* Traverses the tree in Postorder direction.
         */
        void traversePostorder();

        /* Finds the node in the tree containing the
         * value "element".
         *
         * @param  element  The content of the Node-to-find
         * @return The node in the tree containing this
         *         value, NULL otherwise
         */
        Node *findNode( int element );

        /* Removes a Node with content element
         * from the tree, so it firstly searches
         * for it and then calls the appropiate
         * helper method to delete it.
         * Note: we have to decide how to delete
         *       the Node if it is a leaf, an
         *       internal Node or the root.
         *
         * @param  element  The element to remove.
         * @return True if the element was found
         *         and removed, False otherwise
         */
        void remove( int element );

        /* Prints the tree to stdout.
         */
        void printTree();
};
