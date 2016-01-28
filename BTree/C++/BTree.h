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

#ifndef _BTREE_H
#define _BTREE_H

#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>
using std::ostream;
using std::vector;

/**
 * Implementation of a B-tree using parametric polymorphism.<p>
 * A B-tree is a tree data structure designed for holding a very large amount 
 * of values while still ensuring that access is performant. This is ensured
 * by having nodes that can hold much more than only a single value and a node
 * can have more than only two children.<p>
 * Consider the following example:
 * The access time for searching in a binary search tree containing {@code n}
 * nodes is {@code log_2(n)}, so a search in a tree containing one million
 * nodes it would take {@code log_2(1,000,000) &#8776; 20} comparisons. In a
 * B-tree with a degree of 512 would take {@code log_1025(1,000,000) &#8776; 2}
 * comparisons for traversing, about <strong>ten times less</strong>!
 * This is one reason while B-trees are often used in database management 
 * systems. Note that the degree is a constant number, so the times for 
 * searching in a node is negligible.<p>
 * All common operations (searching, insertion, deletion) run in 
 * {@code O(log n)} where {@code n} denotes the size of the tree, both in 
 * average and worst case.
 *
 * @author Thomas Lang
 * @version 1.1, 2016-01-27
 * @see <a href="https://en.wikipedia.org/wiki/B-tree">B-trees on Wikipedia</a>
 * @see #BTree()
 */
template<class T>
class BTree {
    
    /*
     * One comment about the following implementation:
     * I know that here one would usually use a more dynamic internal data
     * structure like a double linked list or something like that. But I wrote
     * this as a proof-of-concept that this can also worky properly with 
     * vectors, although this is not very performant due to many insertions and
     * deletions that force many copying actions.
     *
     * So, do not get way too angry about it, just accept it or do it better ;)
     */

    private:
	    /** The degree of this tree. */
        unsigned int degree;
    protected:
       /**
        * Implementation of a single node of a {@code BTree}.<p>
        * Such a node contains exactly three things:
        *
        * <ol>
        *   <li>The list of values.</li>
        *   <li>The list of children.</li>
        *   <li>A pointer to the parent node.</li>
        * </ol>
        *
        * @author Thomas Lang
        * @version 1.0, 2016-01-26
        */
        template<class U>
        class Node {
            public:
                /** Reference to the surrounding class. */
                BTree<U>* outer;

                /** Container holding the values. */
                vector<U> values;

                /** Container holding references to child nodes. */
                vector<Node<U>*> refs;

                /** Reference to the parent node. */
                Node<U>* parent;

               /**
                * Creates a new node by initializing the lists containing the values
                * of the node, or the children respectively.
                *
                * @param outer 
                *         Reference to the surrounding class for acessing members.
                */
                Node(BTree<U>& outer) : outer(nullptr), parent(nullptr)
                {
                    this->outer = &outer;
                }

                /**
                 * Destroys a node instance.
                 */
                ~Node() {
                    for (auto r = refs.begin(); r != refs.end(); ++r) delete *r;
                    values.clear();
                    refs.clear();
                    parent = nullptr;
                }

                /** 
                 * Assignment operator overload.
                 *
                 * @param rhs The right hand side of the operator.
                 * @return Returns the modified 'this' reference.
                 */
                Node<U>& operator=(const Node<U>& rhs)
                {
                    if (this != &rhs) {
                        delete parent;
                        outer = rhs.outer;
                        parent = rhs.parent;
                        values = rhs.values;
                        refs = rhs.refs;
                    }
                    return *this;
                }

               /**
                * Checks if the node is full.<p>
                * A node is full, if it already contains more than the maximum amount
                * of values, which is exactly two times the degree of the tree, as
                * described above.
                *
                * @return Returns either {@code true} if the node is full or 
                *         {@code false} otherwise.
                * @see BTree#BTree()
                */
                bool isFull() const { return (values.size() > 2 * outer->degree); }

               /**
                * Checks if this node is a leaf node.<p>
                * A node is a leaf node, if it has no children.
                *
                * @return Returns either {@code true} if the node is a leaf node or
                *         {@code false} otherwise.
                */
                bool isLeaf() const {
                    for (auto it = refs.cbegin(); it != refs.cend(); ++it)
                        if (*it)
                            return false;
                    return true;
                }

               /**
                * Checks if this node has an underflow, what means that it contains 
                * less values than the minimum amount of entries (the degree);
                *
                * @return Returns either {@code true} if the node has an underflow or
                *         {@code false} otherwise.
                */
                bool underflow() const { return values.size() < outer->degree; }

               /**
                * Returns a string representation of a single node written into a stream.<p>
                * This representation includes firstly the values of this node itself.
                * Secondly, all children are included enclosed by parentheses.
                * This means that the entire tree can be printed by printing the root.
                * <p>
                * Let's figure this out on a small example tree:
                *
                * <pre>
                * <code>
                *         |2|
                *       /     \
                *   |0|1|     |3|
                * </code>
                * <pre>
                *
                * This tree will be printed as: {@code [2]([0,1])([3])}
                *
                * @param out
                *         The stream to write into.
                * @param src
                *         The node to write into the stream.
                * @return Returns a string representation as described above written into the stream.
                */
                friend ostream& operator<<(ostream& out, const Node& src) {
					out << "[";
					for (auto it = src.values.cbegin(); it != src.values.cend(); ++it) {
						out << *it;
						if ((it + 1) < src.values.cend())
							out << ", ";
					}
					out << "]";

					for (auto it = src.refs.cbegin(); it != src.refs.cend(); ++it)
						if (*it) 
							out << "(" << *(*it) << ")";
		
					return out;
                }
        };

        friend class Node<T>;

        Node<T>* get(Node<T>*, T) const;
        void deleteFromRoot(Node<T>*);
        void rebalance(Node<T>*);
        void rebalanceAfterDeletion(Node<T>*);
    public:
        BTree(const int);
        virtual ~BTree();
        BTree<T>& operator=(const BTree<T>&);
        bool insert(T);
        bool is_empty() const;
        bool remove(T);
        void clear();
        unsigned int size() const;
        template<class U> friend ostream& operator<<(ostream&, const BTree<U>&);
    private:
	    /** The size of the tree. */
        unsigned int _size;
	
	    /** Pointer to the root node. */
        Node<T>* root;
};

/**
 * Creates a new {@code BTree} with the passed degree.<p>
 * The degree of a B-tree is here defined as follows:
 * The result of {@code 2 * degree} is the maximum size of a single node
 * before splitting. So, a degree of {@code 1} means that a single node can
 * hold a maximum of 2 values, if a third value will be added, then this
 * node is splitted up into three nodes. As a consequence, a single node
 * can hold a maximum of {@code 2 * degree + 1} references to its children.
 * On the other hand, the degree defines the minimum amount of elements
 * stored in a single node, so if a node goes below this limit (e.g. in a
 * deletion), the tree will be rebalanced. An exception to this rule is the
 * root node.
 *
 * @param _degree
 *          The degree of this B-tree.
 * @see #insert(T)
 */
template<class T>
BTree<T>::BTree(const int _degree)
    : degree(_degree)
    , _size(0)
    , root(nullptr)
{
}

/**
 * Destroys a BTree instance.
 */
template<class T>
BTree<T>::~BTree()
{
    delete root;
}

/**
 * Assignment operator overload.
 *
 * @param rhs 
 *         The right hand side of the operator to copy.
 * @return Returns the modified BTree instance.
 */
template<class T>
BTree<T>& BTree<T>::operator=(const BTree<T>& rhs)
{
    if (this != &rhs) {
        degree = rhs.degree;
        _size = rhs._size;
        delete root;
        root = nullptr;
        root = rhs.root;
    }
    return *this;
}

/**
 * Checks if this tree is empty or not.
 *
 * @return Returns either {@code true} if the tree has no elements, 
 *         {@code false} otherwise.
 */
template<class T>
bool BTree<T>::is_empty() const
{
    return (_size == 0);
}

/**
 * Clears the tree.
 */
template<class T>
void BTree<T>::clear()
{
    for (auto it = root->refs.begin(); it != root->refs.end(); ++it) {
        delete *it;
        *it = nullptr;
    }
    root->values.clear();
    root->refs.clear();
    _size = 0;
}

/**
 * Returns the amount of values stored in this tree.
 *
 * @return Returns the size of the tree.
 */
template<class T>
unsigned int BTree<T>::size() const
{
    return _size;
}

/**
 * Inserts the passed {@code value} into this tree.<p>
 * If the tree is empty, then the value becomes a single node and the root
 * of the tree. If the tree is not empty, then the node is searched, where
 * the value should be inserted to.
 * When we got this node, we insert the value and rebalance the tree if
 * necessary.
 *
 * @param value
 *         The value to insert into the tree.
 * @return Returns {@code true} if the insertion proceeded successfully or
 *         {@code false} otherwise.
 * @see #get(Node<T>, T)
 * @see #rebalance(Node<T>)
 */
template<class T>
bool BTree<T>::insert(T value)
{
    if (is_empty()) {
        root = new Node<T>(*this);
        root->values.push_back(value);
        root->refs.push_back(nullptr);
        root->refs.push_back(nullptr);
        ++_size;
        return true;
    }

    Node<T>* const node = get(root, value);

    auto valIt = node->values.begin();
    auto refIt = node->refs.begin();
    refIt++;
    unsigned int index = 0;

    while (valIt != node->values.end()) {
        T itValue = *valIt;

        if (value < itValue) {
            node->values.insert(valIt - 1, value);
            node->refs.insert(refIt - 1, nullptr);
            break;
        } else if (value == itValue) {
            /* No duplicates allowed. */
            return false;
        } else if ((value > itValue) 
                && ((index + 1) == node->values.size())) {
            /*
             * Here is the special case that the value is greater than 
             * every other value in the node where the iterator would fail.
             */
            node->values.push_back(value);
            node->refs.push_back(nullptr);
            break;
        }
        ++index;
        ++valIt;
    }

    if (node->isFull()) {
        rebalance(node);
    }

    ++_size;

    return false;
}

/**
 * Navigates through the tree and returns the node where the passed 
 * {@code value} should be inserted to or that already contains this value.
 * Note that this function <em>never</em> returns {@code null}.
 * <p>
 * This traversion follows the following rules:<p>
 * Lets think about the following example node:
 *
 * <pre>
 * <code>
 *     | 1 | 2 | 3 |
 *    ^   ^^
 * </code>
 * </pre>
 *
 * So if the value is contained in the node (remember that in a B-tree also
 * final values can be contained in an internal node, not only in the leaf)
 * then this node is returned. Otherwise, the next node to search in 
 * depends on the value:
 * <ol>
 *   <li>
 *     If the value is smaller than the first element (here {@code 1}) then
 *     the next node to search in is the reference denoted with {@code ^}.
 *   </li>
 *   <li>
 *     If the value is <em>strictly</em> greater than the first element 
 *     ({@code 1}) and <em>strictly</em> smaller than the next element
 *     ({@code 2}), the next node to search in is the reference denoted
 *     with {@code ^^}.
 *   </li>
 *   <li>
 *     And so on for all references. If the value is greater than all
 *     values in the node, the next node to search in is the very last 
 *     reference in the node.
 *   </li>
 * </ol>
 *
 * @param current
 *         The current node to look in.
 * @param value
 *         The value to search in the nodes for.
 * @return Returns the node that either contains the passed value or where
 *         this value should be inserted into.
 * @throws 0
 *         0 will be thrown if no node can be detected, as this state can 
 *         never occur in a B-tree.
 */
template<class T>
typename BTree<T>::template Node<T>* BTree<T>::get(Node<T>* current, T value) const
{
    Node<T>* node = nullptr;
    auto refIt = current->refs.cbegin();

    if (refIt == current->refs.cend())
        /* If there are no child references, this is the wanted node. */
        return current;

    unsigned int index = 0;

    /* 
     * If there are child references we find the node by traversing as 
     * described in the above documentation of this function.
     */
    for (auto valIt = current->values.cbegin(); valIt != current->values.cend(); ++valIt, ++refIt, ++index) {
        const T curValue = *valIt;
        Node<T>* child = *refIt;

        if (value < curValue) {
            if (child)
                return get(child, value);
            else {
                node = current;
                break;
            }
        } else if ((value > curValue) 
                && ((index + 1) == current->values.size())) {
            Node<T>* n = current->refs.back();

            if (n)
                return get(n, value);
            else {
                /* Equality, we found the value itself. */
                node = current;
                break;
            }
        } else if (value == curValue) {
            node = current;
            break;
        }
    }

    /*
     * To all programmers doomed to debug this code:
     * Be aware of the beauty and cruelty of the below thing, as everyone provoking
     * such an error has an IQ of 0, they should get their own medicine back!
     */
    if (!node)
        throw 0;

    return node;
}

/**
 * Rebalances the tree starting at {@code node}.<p>
 * Such a rebalancing only happens, if the passed node is full.
 * If so, the passed node will be split up into two nodes. The first one 
 * contains all values and references to children with values 
 * <em>smaller</em> than the <strong>median</strong> of all values in the
 * node. The second one analogous contains all values and references 
 * <em>greater</em> than the median of all values. If the passed node was
 * the root, then the median will become the new root, else the median will
 * be inserted into the parent node and the rebalancing continues on the
 * parent node.
 *
 * @param node
 *         The node to start rebalancing at which must not be {@code null}.
 */
template<class T>
void BTree<T>::rebalance(Node<T>* node)
{
    assert(node != nullptr);

    if (!node->isFull())
        /* If the node is not full, there is nothing to rebalance. */
        return;

    /*
     * First, find the median of all values of the node and create the left
     * and right children.
     *
     * As the values in nodes are already sorted, it is easy to determine 
     * this things.
     */
    int medianIndex = (int) ceil(node->values.size() / 2);
    T median = node->values[medianIndex];

    Node<T>* leftChild = new Node<T>(*this);
    Node<T>* rightChild = new Node<T>(*this);

    leftChild->values.insert(leftChild->values.begin(), node->values.begin(), node->values.begin() + medianIndex);
    leftChild->refs.insert(leftChild->refs.begin(), node->refs.begin(), node->refs.begin() + medianIndex + 1);
    rightChild->values.insert(rightChild->values.begin(), node->values.begin() + medianIndex + 1, node->values.end());
    rightChild->refs.insert(rightChild->refs.begin(), node->refs.begin() + medianIndex + 1, node->refs.end());

    if (node == root) {
        /* If the node was the root, we'll create a new root. */
        Node<T>* newRoot = new Node<T>(*this);
        newRoot->values.push_back(median);
        newRoot->refs.push_back(leftChild);
        newRoot->refs.push_back(rightChild);
        leftChild->parent = newRoot;
        rightChild->parent = newRoot;

        /* 
         * If we create a new node, it may be that the parent pointers of
         * the children contain old references, so we must refresh them.
         */
        for (auto lrit = leftChild->refs.begin(); lrit != leftChild->refs.end(); ++lrit)
            if (*lrit)
                (*lrit)->parent = leftChild;

        for (auto rrit = rightChild->refs.begin(); rrit != rightChild->refs.end(); ++rrit)
            if (*rrit)
                (*rrit)->parent = rightChild;

        root = newRoot;

        for (auto it = node->refs.begin(); it != node->refs.end(); ++it)
            *it = nullptr;
        node->parent = nullptr;
        delete node;
        return;
    }

    Node<T>* const parent = node->parent;
    auto valIt = parent->values.begin();
    auto refIt = parent->refs.begin();

    if (parent->refs.back() == node) {
        /*
         * Here is the special case that the node to split is the outermost
         * node, where the iterator would fail.
         */
        parent->refs.pop_back();
        parent->refs.push_back(leftChild);
        parent->refs.push_back(rightChild);
        parent->values.push_back(median);
    } else {
        int index = 0;

        while ((valIt != parent->values.end()) && (refIt != parent->refs.end())) {
            Node<T>* curNode = *refIt;

            if (curNode == node) {
                parent->refs[index] = leftChild;
                parent->refs.insert(parent->refs.begin() + index + 1, rightChild);
                parent->values.insert(valIt - 1, median);
                break;
            }
            ++valIt;
            ++refIt;
            ++index;
        }
    }

    leftChild->parent = parent;
    rightChild->parent = parent;
        
    for (auto lrit = leftChild->refs.begin(); lrit != leftChild->refs.end(); ++lrit)
        if (*lrit)
            (*lrit)->parent = leftChild;

    for (auto rrit = rightChild->refs.begin(); rrit != rightChild->refs.end(); ++rrit)
        if (*rrit)
            (*rrit)->parent = rightChild;

    
    for (auto it = node->refs.begin(); it != node->refs.end(); ++it)
        *it = nullptr;
    node->parent = nullptr;
    delete node;

    /*
     * The insertion of the median into the parent node may cause the 
     * parent node to become overfull, so we have to check if we must 
     * rebalance the parent node again.
     */
    rebalance(parent);
}

/**
 * Removes the element {@code value} from the tree.<p>
 * If the tree is empty then nothing happens. Otherwise the node 
 * containing the element will be searched. Then the deletion happens.
 * If necessary, the node where the element was deleted will be rebalanced.
 *
 * @param value
 *         The value to delete from the tree.
 * @return Returns either {@code true} if the deletion was successful or
 *         {@code false} if not.
 */
template<class T>
bool BTree<T>::remove(T value)
{
    if (is_empty())
        return false;

    Node<T>* const node = get(root, value);
    bool contains = false;
    auto cValIt = node->values.cbegin();

    for (; cValIt != node->values.cend(); ++cValIt)
        if (*cValIt == value) {
            contains = true;
            break;
        }

    if (!contains)
        return false;

    if (node->isLeaf()) {
        /* If the node is a leaf node, deletion is simple. */
        node->values.erase(cValIt);
        node->values.shrink_to_fit();

        if (node->underflow())
            rebalanceAfterDeletion(node);
    } else {
        /*
         * If the node is an internal node, we have to replace the 
         * separator with the maximum node still below it to ensure the
         * constraints to such a tree.
         *
         * So the node where something is actually deleted is in this 
         * implementation the nearest left child to the node. This node
         * must be rebalanced if necessary.
         */
        Node<T>* left = nullptr;
        auto refIt = node->refs.begin();
        unsigned int index = 0;

        while (refIt != node->refs.end()) {
            Node<T>* curNode = *refIt;

            if (curNode) {
                T maxValue = curNode->values.back();
                Node<T>* nextNode = *(refIt + 1);
                T nextValue = nextNode->values.front();

                if ((maxValue < value) && (nextValue > value)) {
                    left = curNode;
                    curNode->values.pop_back();
                    curNode->values.shrink_to_fit();
                    node->values[index] = maxValue;
                    break;
                }
            }
            ++refIt;
        }

        if (left && left->underflow())
            rebalanceAfterDeletion(left);
    }

    return true;
}

/**
 * Rebalances the tree starting at {@code node} after a deletion.<p>
 * If the passed node has an underflow due to a deletion, it must be 
 * rebalanced. There are three possible ways to do so depending on the
 * size of the node's immediate siblings:
 *
 * <ol>
 *  <li>
 *    If the node's direct right sibling exists and can share an element:
 *    <ol>
 *      <li>
 *        Move the separator down to the end of the node.
 *      </li>
 *      <li>
 *        Replace the old separator in the parent node with the first 
 *        element from the right sibling.
 *      </li>
 *      <li>
 *        After this, the tree is finished as the right sibling cannot have
 *        an underflow because we checked earlier that it can share at 
 *        least one element.
 *      </li>
 *    </ol>
 *  </li>
 *  <li>
 *    Otherwise, if the node's direct left sibling exists and can share an
 *    element:
 *    <ol>
 *      <li>
 *        Move the separator down to the beginning of the node.
 *      </li>
 *      <li>
 *        Replace the old separator in the parent node with the last 
 *        element from the left sibling.
 *      </li>
 *      <li>
 *        After this, the tree is finished as the left sibling cannot have
 *        an underlow because we checked earlier taht it can share at least
 *        one element.
 *      </li>
 *    </ol>
 *  </li>
 *  <li>
 *    Otherwise:
 *    <ol>
 *      <li>
 *        Move down the separator to the end of the left node that may be
 *        a sibling of the deficient node itself.
 *      </li>
 *      <li>
 *        Move <em>all</em> elements from the right sibling to the left
 *        node (including the pointers to it's children). As at this point
 *        the left node becomes full while the right node becomes empty.
 *      </li>
 *      <li>
 *        Remove the separator from the parent node including it's right
 *        pointer to the right node. Here we must distinguish between two
 *        possible cases:
 *        <ul>
 *          <li>
 *            If the parent node is the root and if this does not contain
 *            any elements now, just make the merged node (the original 
 *            left node) the new root.
 *          </li>
 *          <li>
 *            Otherwise, the parent node may have an underflow now, so this
 *            node may be rebalanced.
 *          </li>
 *        </ul>
 *      </li>
 *    </ol>
 *  </li>
 * </ol>
 */
template<class T>
void BTree<T>::rebalanceAfterDeletion(Node<T>* node)
{
    assert (node != nullptr);

    if (!node->underflow())
        return;

    Node<T>* parent = node->parent;
    Node<T>* leftSibling = nullptr, *rightSibling = nullptr;
    int leftSiblingIndex = -1, rightSiblingIndex = -1;
    auto refIt = parent->refs.begin();
    T separator;
    unsigned int separatorIndex = 0;

    if (parent->refs.back() == node) {
        /*
         * Here is the special case that the current node is the outermost
         * node of its parent.
         */
        leftSiblingIndex = parent->refs.size() - 2;
        rightSiblingIndex = parent->refs.size() - 1;
        leftSibling = parent->refs[leftSiblingIndex];
        rightSibling = parent->refs[rightSiblingIndex];
        separator = parent->values.back();
        separatorIndex = parent->values.size() - 1;
    } else {
        unsigned int refIndex = 0;
        bool nullSeparator = true;

        /* Find the separator and the two siblings. */
        while (refIt != parent->refs.end()) {
            Node<T>* const curNode = *refIt;

            if (curNode == node) {
                const unsigned int leftIndex = refIndex;
                const unsigned int rightIndex = refIndex + 1;

                if ((leftIndex >= 0) && (leftIndex < parent->refs.size())) {
                    leftSiblingIndex = leftIndex;
                    leftSibling = parent->refs[leftSiblingIndex];
                    separator = parent->values[leftSiblingIndex];
                    nullSeparator = false;
                }

                if ((rightIndex > 0) && (rightIndex - 1 < parent->refs.size())) {
                    rightSiblingIndex = rightIndex;
                    rightSibling = parent->refs[rightSiblingIndex];
                    if (!nullSeparator) separator = parent->values[rightSiblingIndex - 1];
                }

                break;
            }

            ++refIndex;
            ++separatorIndex;
        }
    }

    /* 
     * Rebalancing strategies, check out JavaDoc comment for documentation.
     */

    if (rightSibling && (rightSibling->values.size() > degree)) {
        /* First strategy: rotate left */
        Node<T>* const right = rightSibling->refs.front();
        node->values.push_back(separator);
        node->refs.push_back(right);

        if (right)
            right->parent = node;

        parent->values.erase(parent->values.begin() + separatorIndex);
        parent->values.push_back(rightSibling->values[0]);
        rightSibling->values.erase(rightSibling->values.begin());
        rightSibling->refs.erase(rightSibling->refs.begin());
        parent->values.shrink_to_fit();
        rightSibling->values.shrink_to_fit();
        rightSibling->refs.shrink_to_fit();
    } else if (leftSibling && leftSibling->values.size() > degree) {
        /* Second strategy: rotate right */
        Node<T>* const left = leftSibling->refs.back();
        node->values.insert(node->values.begin(), separator);
        node->refs.insert(node->refs.begin(), left);

        if (left)
            left->parent = node;

        parent->values.erase(parent->values.begin() + separatorIndex);
        parent->values.insert(parent->values.begin(), leftSibling->values.back());
        leftSibling->values.pop_back();
        leftSibling->refs.pop_back();
        parent->values.shrink_to_fit();
        leftSibling->values.shrink_to_fit();
        leftSibling->refs.shrink_to_fit();
    } else if (leftSibling && rightSibling) {
        /* Third strategy: Merge siblings. */
        leftSibling->values.push_back(separator);
        auto valIt = rightSibling->values.begin();
        auto refIt = rightSibling->refs.begin();

        for (; valIt != rightSibling->values.end(); ++valIt)
            leftSibling->values.push_back(*valIt);
        for (; refIt != rightSibling->refs.end(); ++refIt)
            leftSibling->refs.push_back(*refIt);
        parent->values.erase(parent->values.begin() + rightSiblingIndex - 1);
        parent->refs.erase(parent->refs.begin() + rightSiblingIndex);

        parent->values.shrink_to_fit();
        parent->refs.shrink_to_fit();
        rightSibling = nullptr;

        if ((parent == root) && (parent->refs.empty()))
            root = leftSibling;
        else if (parent->underflow())
            rebalanceAfterDeletion(parent);
    }
}

/**
 * Returns a string representation of a BTree written into a stream.
 *
 * @param out 
 *         The stream to write into.
 * @param src 
 *         The BTree to write.
 * @return Returns the string representation written into the stream.
 */
template<class T>
ostream& operator<<(ostream& out, const BTree<T>& src)
{
    if (src.is_empty())
        out << "(empty tree)";
    else
        out << *(src.root);
    return out;
}

#endif
