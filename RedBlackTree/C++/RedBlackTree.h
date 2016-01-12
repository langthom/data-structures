/* Copyright (c) 2015, Thomas Lang. All rights reserved.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */

#ifndef _RED_BLACK_TREE
#define _RED_BLACK_TREE

#include <ostream>
#include <cassert>
#include "RedBlackTree.h"
using std::ostream;

/**
 * Implementation of a red-black tree.<p>
 * A red-black tree is a binary search tree whose nodes can have colors (either
 * red or black). This tree is self-balancing and guarantees that all important
 * operations (like searching, inserting and removing values) run in 
 * {@code O(log n)} where {@code n} denotes the number of elements in the tree.
 * <p>
 * This rebalancing is done by guaranteeing the following rules:
 * <ul>
 *  <li>
 *    The root is always black.
 *  </li>
 *  <li>
 *    The leaf nodes (what are {@code null} pointers in reality) are black.
 *  </li>
 *  <li>
 *    If a node is red, both children are black.
 *  </li>
 *  <li>
 *    Every path from a node to its children contain the same amount of black 
 *    nodes.
 *  </li>
 * </ul>
 *
 * @author Thomas Lang
 * @version 1.0, 2016-01-12
 * @see Node
 */
template<class T>
class RedBlackTree {
    public:
        /**
         * Creates a new RedBlackTree.
         */
        RedBlackTree();

        /**
         * Destroys a RedBlackTree instance.
         */
        virtual ~RedBlackTree();

		/**
		 * Checks if the tree is empty or not.
		 *
		 * @return Returns either {@code true} if the tree is empty or
		 *         {@code false} otherwise.
		 */
        bool isEmpty() const;

		/**
		 * Returns the number of elements stored in the tree.
		 *
		 * @return Returns the size of the tree.
		 */
        int size() const;

		/**
		 * Clears the tree.
		 */
        void clear();

		/**
		 * Inserts the passed value into the tree.
		 *
		 * @param value
		 *         The value to insert into the tree which must not be 
		 *         {@code null}.
		 * @return Returns either {@code true} if the insertion was successful
		 *         or {@code false} otherwise.
		 * @throws NullPointerException
		 *         A {@code NullPointerException} will be thrown if the passed
		 *         value was {@code null}.
		 */
        bool insert(T);

		/**
		 * Checks if the tree contains the passed {@code value}.
		 *
		 * @param value
		 *         The value to search for.
		 * @return Returns either {@code true} if the tree contains the passed
		 *         {@code value} or {@code false} otherwise.
		 * @see #get(T)
		 */
        bool contains(T) const;

		/**
		 * Removes the passed {@code value} from this tree.<p>
		 * If necessary, the tree will be rebalanced after the removal. If the 
		 * passed {@code value} does not exist in the tree, this function does
		 * nothing and returns {@code false}.
		 *
		 * @param value
		 * @return Returns {@code true} if the removal was successful or 
		 *         {@code false} otherwise.
		 * @see #deleteInternalNode(Node<T>)
		 * @see #deleteNode(Node<T>)
		 */
        bool remove(T);

        /**
         * Writes a pretty-print version of a RedBlackTree instance on the stream.
         *
         * @param ostream The stream to write.
         * @param src The tree to write.
         * @return Returns the stream.
         */
        template<class U>
        friend std::ostream& operator<<(std::ostream&, const RedBlackTree<U>&);
    private:
		/** Enum representing the two possible colors of a node. */
        enum Color { RED, BLACK };

		/**
		 * Implementation of a single node of a Red-black tree.<p>
		 * This is basically a simple node of any binary search tree but with
		 * the additional {@code color} attribute of type {@code Color}, what 
		 * can be either {@code Color.RED} or {@code Color.BLACK}.
		 *
		 * @author Thomas Lang
		 * @version 2016-01-12
		 * @see Color
		 */
        class Node {
            public:
				/**
				 * Creates a new red node encapsulating the passed {@code value}.
				 *
				 * @param value
				 *          The value encapsulated in this node.
				 */
                Node(T _value) 
                  : value(_value)
                  , left(nullptr)
                  , right(nullptr)
                  , parent(nullptr) 
                {}

                /**
                 * Destroys the node and all subnodes.
                 */
                virtual ~Node() {
                    delete left;
                    delete right;
                }

                /**
                 * Writes the node instance to the stream.
                 *
                 * @param out The stream to write to.
                 * @param src The node instance.
                 * @return Returns the stream.
                 */
                friend ostream& operator<<(ostream& out, const Node& src) {
                    if (src.left)
                        out << "(" << *(src.left) << ")";
                    std::string col = (src.color == RED) ? "RED" : "BLACK";
                    out << "[" << col << ": " << src.value << "]";
                    if (src.right)
                       out << "(" << *(src.right) << ")";
                    return out;
                }

                Color color;
                T value;
                Node* left;
                Node* right;
                Node* parent;
        };

		/**
		 * Gets the node containing the passed {@code value}.
		 *
		 * @param value
		 *         The value to search for.
		 * @return Returns either the node containing the passed {@code value} or
		 *         {@code null} if the tree is empty or no node could be found.
		 * @see #get(Node<T>, T)
		 */
        Node* get(T);

		/**
		 * Recursively gets the node containing the passed {@code value}.
		 *
		 * @param current
		 *         The current traversal node, which must not be {@code null}.
		 * @param value
		 *         The value to search for.
		 * @return Returns either the node containing the passed {@code value} or
		 *         {@code null} if no such node could be found.
		 */
        Node* get(Node*, T);

		/**
		 * Recursively traverses through the tree until the position for inserting
		 * is found. Then the node is inserted. If necessary, the tree is also 
		 * rebalanced starting from the new node.
		 *
		 * @param current
		 *         The current traversal node which must not be {@code null}.
		 * @param node
		 *         The node to insert into the tree which must not be {@code null}.
		 * @return Returns either {@code true} if the insertion was successful or
		 *         {@code false} if the node already exists in the tree.
		 * @see #rebalance(Node<T>)
		 */
        bool insert(Node*, Node*);

		/**
		 * Deletes a node from the tree that has both two children.
		 *
		 * @param node
		 *          The node to delete which must not be {@code null}.
		 * @see #deleteNode(Node<T>)
		 */
        void deleteInternalNode(Node*);

		/**
		 * Deletes a node from the tree that does not have two real children.
		 * The tree will be rebalanced if necessary.
		 *
		 * @param node
		 *          The node to delete which must not be {@code null}.
		 * @see #rebalanceAfterDeletion(Node<T>)
		 */

        void deleteNode(Node*);

		/**
		 * Rebalances the tree from {@code node} if necessary.
		 *
		 * @param node
		 *          The node to start rebalancing from.
		 */
        void rebalance(Node*);

		/**
		 * Rebalances the tree after a deletion.
		 *
		 * @param node
		 *          A child of the deleted node which must not be {@code null}.
		 */
        void rebalanceAfterDeletion(Node*);

		/**
		 * Performs a single left rotation around the passed {@code node}.
		 *
		 * @param node
		 *          The node to rotate around which must not be {@code null}.
		 */
        void rotateLeft(Node*);

		/**
		 * Performs a single right rotation around the passed {@code node}.
		 *
		 * @param node
		 *          The node to rotate about which must not be {@code null}.
		 */
        void rotateRight(Node*);
    private:
        int _size;
        Node* root;
};


//------------------------------------------------------------------------------

template<class T>
RedBlackTree<T>::RedBlackTree()
    : _size(0)
    , root(nullptr)
{
}

template<class T>
RedBlackTree<T>::~RedBlackTree()
{
    std::cout << "In tree destructor" << std::endl;
    delete root;
}

template<class T>
bool RedBlackTree<T>::isEmpty() const
{
    return _size == 0;
}

template<class T>
int RedBlackTree<T>::size() const
{
    return _size;
}

template<class T>
bool RedBlackTree<T>::insert(T value)
{
    Node* node = new Node(value);
    bool success = true;

    if (isEmpty()) {
        node->color = BLACK;
        root = node;
    } else
        success = insert(root, node);

    if (success)
        ++_size;

    return success;
}

template<class T>
bool RedBlackTree<T>::insert(Node* current, Node* node)
{
    assert (current != nullptr);
    assert (node != nullptr);

    const T curValue = current->value;
    const T newValue = node->value;
    bool success = true;

    if (newValue < curValue) {
        if (!current->left) {
            current->left = node;
            node->parent = current;
            rebalance(node);
        } else
            success = insert(current->left, node);
    } else if (newValue > curValue) {
        if (!current->right) {
            current->right = node;
            node->parent = current;
            rebalance(node);
        } else
            success = insert(current->right, node);
    } else
        success = false; /* no duplicates allowed */

    return success;
}

template<class T>
void RedBlackTree<T>::rebalance(Node* node)
{
    assert (node != nullptr);

    Node* parent = node->parent;

    if (!parent) {
		/*
		 * Case 1: The new node has no parent. This means that the new node
		 * is the root and the root always must be black.
		 */
       node->color = BLACK;
        return;
    }

    if (parent->color == BLACK) 
        /*
         * Case 2: Per default every new node (including this one) are red.
         * When the color of the parent node is black, then the depth of
         * black nodes is still the same and we do not have to do anything.
         */
        return;

    Node* grandParent = parent->parent;
    Node* uncle = (parent == grandParent->left) ? grandParent->right : grandParent->left;

    if (uncle && (uncle->color == RED)) {
        /*
         * Case 3: Both the uncle and the parent nodes are red. Then we 
         * restore the tree by changing the below colors what makes the
         * tree be okay locally. But now, the grand parent will be
         * problematic, so we rebalance it.
         */
        parent->color = BLACK;
        uncle->color = BLACK;
        grandParent->color = RED;
        rebalance(grandParent);
    } else {
        /*
         * Case 4: The parent node and the node itself are red and the
         * path from the grand parent to the node forms a zig-zag line.
         * Then we perform a rotation and swap positions what will result
         * in a constellation useable for the fifth case.
         * The exact rotation depends on if the node was a left or a right
         * child.
         */
        if ((node == parent->right) && (parent == grandParent->left)) {
            rotateLeft(parent);
            node = node->left;
        } else if ((node == parent->left) && (parent == grandParent->right)) {
            rotateRight(parent);
            node = node->right;
        }

        /*
         * Case 5: From this position we restore the tree by swapping
         * colors and rotations around the grand parent, depending on if
         * the node was a left or a right child.
         */
        parent = node->parent;
        grandParent = parent->parent;
        parent->color = BLACK;
        grandParent->color = RED;

        if ((node == parent->left) && (parent == grandParent->left))
            rotateRight(grandParent);
        else
            rotateLeft(grandParent);
    }
}

template<class T>
void RedBlackTree<T>::rotateLeft(Node* node)
{
    assert (node != nullptr);

    Node* parent    = node->parent;
    Node* right     = node->right;
    Node* rightLeft = right->left;

    node->parent  = right;
    node->right   = rightLeft;
    right->parent = parent;
    right->left   = node;

    if (rightLeft)
        rightLeft->parent = node;

    if (!parent)
        root = right;
    else if (parent->left == node)
        parent->left = right;
    else
        parent->right = right;
}

template<class T>
void RedBlackTree<T>::rotateRight(Node* node)
{
    assert (node != nullptr);

    Node* parent    = node->parent;
    Node* left      = node->left;
    Node* leftRight = left->right;

    node->parent = left;
    node->left   = leftRight;
    left->parent = parent;
    left->right  = node;

    if (leftRight)
        leftRight->parent = node;

    if (!parent)
        root = left;
    else if (parent->right == node)
        parent->right = left;
    else
        parent->left = left;
}

template<class T>
bool RedBlackTree<T>::contains(T value) const
{
    return get(value) != nullptr;
}

template<class T>
typename RedBlackTree<T>::Node* RedBlackTree<T>::get(T value)
{
    return isEmpty() ? nullptr : get(root, value);
}

template<class T>
typename RedBlackTree<T>::Node* RedBlackTree<T>::get(Node* current, T value)
{
    assert (current != nullptr);

    const T curValue = current->value;

    if (value == curValue)
        return current;
    else if ((value < curValue) && current->left)
        return get(current->left, value);
    else if ((value > curValue) && current->right)
        return get(current->right, value);
    return nullptr;
}

template<class T>
bool RedBlackTree<T>::remove(T value)
{
    Node* node = get(value);

    if (!node)
        return false;

    if (value == root->value) {
        if (!root->left && !root->right) {
            delete root;
            root = nullptr;
            return true;
        } else if (root->left && !root->right) {
            Node* n = root;
            Color c = root->color;
            root = root->left;
            root->color = c;
            n->left = nullptr;
            delete n;
        } else if (!root->left && root->right) {
            Node* n = root;
            Color c = root->color;
            root = root->right;
            root->color = c;
            n->right = nullptr;
            delete n;
        } else
            deleteInternalNode(root);
    } else if (node->left && node->right)
        deleteInternalNode(node);
    else
        deleteNode(node);

    --_size;
    return true;
}

template<class T>
void RedBlackTree<T>::deleteInternalNode(Node* node)
{
    assert (node != nullptr);

    /*
     * The node is deleted by exchanging its value with the largest value
     * from the left sub tree and finally deleting the maximum node of the
     * left sub tree.
     */
    Node* maxOfMin = node->left;

    while (maxOfMin->right)
        maxOfMin = maxOfMin->right;

    node->value = maxOfMin->value;
    deleteNode(maxOfMin);
}

template<class T>
void RedBlackTree<T>::deleteNode(Node* node)
{
    assert (node != nullptr);
    Node* parent = node->parent;
    Node* left   = node->left;
    Node* right  = node->right;
    Node* pright = parent->right;
    bool rChild = pright && (pright->value == node->value);

    /*
     * Please note that we need not to check if 'parent' is null here,
     * because this can only happen if 'node' is the root, but this special
     * case is already recognized in the methode 'remove'.
     */
    if (!node->left && !node->right) {
        if (node->color == BLACK)
            rebalanceAfterDeletion(node);
        if (rChild)
            parent->right = nullptr;
        else
            parent->left = nullptr;
    } else if (node->left) {
        if (rChild)
            parent->right = left;
        else
            parent->left = left;
        left->parent = parent;
    } else {
        if (rChild) {
            parent->right = right;
            parent->right->color = node->color;
        } else {
            parent->left = right;
            parent->left->color = node->color;
        }
        right->parent = parent;
    }

    node->parent = nullptr;
    node->left = nullptr;
    node->right = nullptr;
    delete node;
}

template<class T>
void RedBlackTree<T>::rebalanceAfterDeletion(Node* node)
{
    assert (node != nullptr);
    Node* parent = node->parent;

    if (!parent)
        /* Case 1: Problematic node is root, no rotations to made. */
        return;

    Node* sibling = (node == parent->left) ? parent->right : parent->left;

    if (sibling->color == RED) {
        /*
         * Case 2: The sibling of the node is red.
         * Then invert the colors of the parent and the sibling node
         * following by performing a left / right rotation around the
         * parent node depending on if the node was a left or a right
         * child.
         */
        parent->color = RED;
        sibling->color = BLACK;

        if (node == parent->left)
            rotateLeft(parent);
        else
            rotateRight(parent);
    }

    const Color pNode = parent->color;
    const Color sNode = sibling->color;
    const Color slNode = !sibling->left ? BLACK : sibling->left->color;
    const Color srNode = !sibling->right ? BLACK : sibling->right->color;

    if ((pNode == BLACK) && (sNode == BLACK) && (slNode == BLACK) && (srNode == BLACK)) {
        /*
         * Case 3: The parent, the sibling and both children of the sibling
         * are black. Then the sibling has the wrong color, so change it to
         * red. This may have corrupted any integraty conditions of the
         * parent node, so we have to rebalance the parent node.
         */
        sibling->color = RED;
        rebalanceAfterDeletion(parent);
    } else if ((pNode == RED) && (sNode == BLACK) && (slNode == BLACK) && (srNode == BLACK)) {
        /*
         * Case 4: The sibling and its both children are black but the 
         * parent is red. Then we can rebalance the tree by simply 
         * inverting the colors of the sibling and parent node.
         */
        sibling->color = RED;
        parent->color = BLACK;
    } else {
        /*
         * Case 5:
         * (a): Node is the left child and the sibling and the sibling's
         *      right child are black but the siblings left child is red.
         *      Then we change the colors of the sibling and it's left 
         *      child and perform a right rotation around the sibling.
         *      Then all paths have the same number of right nodes.
         *      After this, we immediately go to case 6.
         * (b): The same thing as in (a) but the other way (right child).
         */
        if ((node == parent->left) && (sNode == BLACK) && (slNode == RED) && (srNode == BLACK)) {
            sibling->color = RED;
            sibling->left->color = BLACK;
            rotateRight(sibling);
        } else if ((node == parent->right) && (sNode == BLACK) && (slNode == BLACK) && (srNode == RED)) {
            sibling->color = RED;
            sibling->right->color = BLACK;
            rotateLeft(sibling);
        }

        /*
         * Case 6: The sibling is black, the right child of the sibling is
         * red and the node is the left child of it's parent.
         * Then we resolve this illegal state by changing the colors as
         * below. After this, we have to correct the now invalid paths
         * by rotating, depending on if the node was a left or a right 
         * child.
         */
        sibling->color = parent->color;
        parent->color = BLACK;

        if (node == parent->left) {
            sibling->right->color = BLACK;
            rotateLeft(parent);
        } else {
            sibling->left->color = BLACK;
            rotateRight(parent);
        }
    }
}

template<class T>
void RedBlackTree<T>::clear()
{
    if (root)
        delete root;
    root = nullptr;
    _size = 0;
}

template<class T>
ostream& operator<<(ostream& out, const RedBlackTree<T>& src)
{
    if (src.isEmpty())
        out << "(empty tree)";
    else
        out << *(src.root);
    return out;
}

#endif
