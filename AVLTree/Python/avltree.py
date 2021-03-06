#!/usr/bin/env python
#
# Copyright (c) 2015, Thomas Lang. All rights reserved.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>
#
#-------------------------------------------------------------------------------


# Implementation of an AVL tree.<p>
# An AVL tree is a binary tree named after its inventors Georgi Maximowitsch 
# <strong>A</strong>delson-<strong>V</strong>elski and Jewgeni Michailowitsch
# <strong>L</strong>andis. This kind of tree is a so called 
# <em>self-balancing</em> tree, that is always as balanced as possible, so
# such a tree cannot get degenerated by insertion or deletions of nodes.
# This in advance guarantees, that search operations, insertions, deletions
# <em>always</em> run in <code>O(log n)</code> with <code>n</code> as the 
# number of nodes in the tree.
#
# @author Thomas Lang
# @version 1.0, 2015-07-28
# @see <a href="https://en.wikipedia.org/wiki/AVL_tree">AVLs on Wikipedia</a>
class AVLTree():
    
    # Indicator that nothing changed to the balance of the tree.#
    NO_CHANGE = 0
    
    # Indicator that the tree is 'one more heavy' on the right side.#
    REBAL_ONE_POS = 1
    
    # Indicator that the tree is 'one more heavy' on the left side.#
    REBAL_ONE_NEG = -1
    
    # Indicator that the tree is 'two more heavy' on the right side.#
    REBAL_TWO_POS = 2
    
    # Indicator that the tree is 'two more heavy' on the left side.#
    REBAL_TWO_NEG = -2

    # Creates a new AVL tree by initializing attributes.
    def __init__(self):
        self.__size = 0
        self.__root = None

    # Inserts a new node with a value of {@code value} into this AVL tree.<p>
    # The insertion itself is just as like the insertion into a normal binary
    # search tree, but after this was successfully performed, a tree rotation
    # is made if necessary, so the tree is in balance again.
    #
    # @param value
    #           The value of the new node to insert.
    # @return Returns {@code true} if the insertion was successful, 
    #         {@code false} otherwise.
    # @see rotate(Node<T>, boolean)
    #
    def insert(self, value):
        node = AVLTree.Node(value)
        __success = True

        if self.isEmpty():
            self.__root = node
        else:
            __success = self.__insert(self.__root, node)

        if __success:
            self.__size += 1

        self.__rotate(node, False)
        return __success

    # Performs the recursive insertion into the tree.<p>
    # This function works <em>exactly</em> like the insertion into a binary
    # search tree and because of that, its runtime lies in {@code O(log n)} 
    # where {@code n} is the number of elements in the tree.
    #
    # @param current
    #           The traversal node used for recursion, that should not be 
    #           {@code null}.
    # @param newNode
    #           The new node to insert, that should not be {@code null}.
    # @return Returns {@code true} if the insertion was successful,
    #         {@code false} otherwise.
    #
    def __insert(self, current, newNode):
        assert (current is not None), "Null node passed."
        assert (newNode is not None), "Null node passed."

        __newValue = newNode.value
        __curValue = current.value

        if __newValue < __curValue:
            if current.left is None:
                current.left = newNode
                newNode.parent = current
            else:
                return self.__insert(current.left, newNode)
        elif __newValue > __curValue:
            if current.right is None:
                current.right = newNode
                newNode.parent = current
            else:
                return self.__insert(current.right, newNode)
        else:
           # No duplicates allowed.
            return False

        return True

    # Performs a tree rotation around the node {@code node}.<p>
    # This method performs a tree rotation around the passed node if 
    # necessary. A rotation must only be performed, if the balance of the
    # node reaches a certain level. The exact behaviour is different, if 
    # it's a rotation for an insertion or if it's a rotation after deleting 
    # a node.:
    # 
    # <ul>
    # <li>
    # If the rotation will be made after an insertion:
    #   <ol>
    #     <li>If the balance is zero, nothing changed to the tree, so there
    #         need not to be any rotation taken.</li>
    #     <li>If the balance is &plusmn; 1, then we must check the balance
    #         of the parent node.</li>
    #     <li>If the balance is &plusmn; 2, then we perform a rotation
    #         depending on this balance and the balance of the previous child.
    #     </li>
    #   </ol>
    # </li>
    # <li>
    # If the rotation will be made after the deletion of a node:
    #   <ol>
    #     <li>If the balance was zero, we must check the balance of the parent
    #         node.</li>
    #     <li>If the balance was &plusmn; 1, then nothing changed on the 
    #         balances of the tree, so no rotation will be performed.</li>
    #     <li>If the balance was &plusmn; 2, then we perform a rotation 
    #         depending on that balance and the one of the deleted node.</li>
    #   </ol>
    # </li>
    # </ul>
    #
    # Note that as a special property of the <em>AVL</em> tree, the balances 
    # of every node <em>always</em> lies in the range of 
    # {@code -1 &leq; x &leq; 1} with {@code x} denoting the balance.
    #
    # @param node
    #          The node to rotate over, which should not be {@code null}.
    # @param deletion
    #          Indicator if this is a rotation after an insertion or after
    #          a deletion of the node.
    # @see <a href="https://en.wikipedia.org/wiki/AVL_tree#Insertion">
    #      Insertions and Deletions in AVL trees on Wikipedia
    #      </a>
    def __rotate(self, node, deletion):
        assert (node is not None), "Null node passed."

        if ((node == self.__root) 
                and (node.left is None) and (node.right is None)):
            # If we have the pure root node here, which does not have any
            # children in this pure case, we need not to rotate anything.
            return

        # Note that if we do not start with the root here, the pointer to the
        # parent node of the passed one is always not null.
        
        __parent = node if deletion else node.parent
        __balance = 0 if __parent is None else __parent.balance

        if __balance == AVLTree.NO_CHANGE:
            if deletion and (__parent.parent is not None):
                self.__rotate(node.parent, deletion)
            else:
                return
        elif abs(__balance) == AVLTree.REBAL_ONE_POS:
            if not deletion and (__parent.parent is not None):
                self.__rotate(node.parent, deletion)
            elif deletion:
                return
        else:
            __lbal = 0 if __parent.left is None else __parent.left.balance
            __rbal = 0 if __parent.right is None else __parent.right.balance

            # Performing rotations depending on the balances of the nodes:
            #
            # (1) If the node parents balance is +2:
            #
            #   (1.1) If the balance of the right child is +1, we perform a 
            #         'left' rotation, so the right child will become the 
            #         parent of its former parent node.
            #
            #   (1.2) If the balance of the right child is -1, we perform a
            #         'right-left' rotation, what is basically a 'right'
            #         rotation on the right child followed by a 'left' 
            #         rotation on the parent node.
            #
            # (2) If the node parents balance is -2:
            #
            #   (2.1) If the balance of the left child is +1, we perform a
            #         'left-right' rotation, what is basically a 'left'
            #         rotation on the left child followed by a 'right'
            #         rotation on the parent node.
            #
            #   (2.2) If the balance of the left child is -1, we perform a
            #         'right' rotation, so the left child will become the
            #         parent of its former parent node.
            if __balance == AVLTree.REBAL_TWO_POS:
                if __rbal == AVLTree.REBAL_ONE_POS:
                    self.__rotateLeft(__parent)
                elif __rbal == AVLTree.REBAL_ONE_NEG:
                    self.__rotateRight(__parent.right)
                    self.__rotateLeft(__parent)
            elif __balance == AVLTree.REBAL_TWO_NEG:
                if __lbal == AVLTree.REBAL_ONE_POS:
                    self.__rotateLeft(__parent.left)
                    self.__rotateRight(__parent)
                elif __lbal == AVLTree.REBAL_ONE_NEG:
                    self.__rotateRight(__parent)

    # Performs a simple 'left' rotation around the node {@code node}.
    #
    # @param node
    #          The node to rotate around, which should not be {@code null}.
    def __rotateLeft(self, node):
        assert (node is not None), "Null node passed."

        __parent       = node.parent
        __right        = node.right
        __right_left   = __right.left

        node.parent    = __right
        node.right     = __right_left

        __right.parent = __parent
        __right.left   = node

        if __right_left is not None:
            __right_left.parent = node

        if __parent is None:
            self.__root = __right
        elif __parent.left == node:
            __parent.left = __right
        else:
            __parent.right = __right

    # Performs a simple 'right' rotation around the node {@code node}.
    #
    # @param node
    #          The node to rotate around, which should not be {@code null}.
    def __rotateRight(self, node):
        assert (node is not None), "Null node passed."

        __parent      = node.parent
        __left        = node.left
        __left_right  = __left.right

        node.parent   = __left
        node.left     = __left_right

        __left.parent = __parent
        __left.right  = node

        if __left_right is not None:
            __left_right.parent = node

        if __parent is None:
            self.__root = __left
        elif __parent.right == node:
            __parent.right = __left
        else:
            __parent.left = __left

    # Retrieves the node with the passed {@code value} from the tree.
    #
    # @param value
    #          The value of the node to return.
    # @return Returns the node with the passed {@code value} from the tree,
    #         or {@code null} if either the tree is empty (has no elements) or
    #         if there is no such node in the tree.
    def get(self, value):
        if self.isEmpty():
            return None
        else:
            return self.__get(self.__root, value)

    # Retrieves the node with the passed {@code value} from the tree 
    # recursively.
    #
    # @param current
    #           The traversal node used for recursion, which should not be
    #           {@code null}.
    # @param value
    #           The value of the node to retrieve.
    # @return Returns either {@code true} if the node with the passed 
    #         {@code value} was found successfully or {@code null} otherwise.
    def __get(self, current, value):
        assert (current is not None), "Null node passed."

        __curValue = current.value

        if value == __curValue:
           # We found the node successfully.
            return current
        elif (value < __curValue) and (current.left is not None):
            return self.__get(current.left, value)
        elif (value > __curValue) and (current.right is not None):
            return self.__get(current.right, value)
        else:
           # We could not find any matching.
            return None

    # Checks if the tree contains a node with the value {@code value}.
    #
    # @param value
    #         The value of the node to check.
    # @return Returns either {@code true} if the tree contains such a node, or
    #         {@code false} if not.
    def contains(self, value):
        return self.get(value) is not None

    # Returns the total amount of nodes in the tree.
    #
    # @return Returns the total amount of nodes in the tree.
    @property
    def size(self):
        return self.__size

    # Removes the node with the passed {@code value} from the tree if
    # existent. As a speciality of an AVL tree, this operation also runs
    # in {@code O(log n)} with {@code n} denoting the number of nodes in the
    # tree. Furthermore, even after a deletion of a node the tree will be 
    # rotated if necessary.
    #
    # @param value
    #         The value of the node to delete.
    # @return Returns either {@code true} if the node was successfully deleted
    #         or {@code false} otherwise.
    def remove(self, value):

        if self.isEmpty():
            return False

        __deletable_node = self.get(value)

        if __deletable_node is None:
           # Node with value 'value' not found.
            return False
        else:
            __parent = __deletable_node.parent

            if value == self.__root.value:
                self.__deleteInternalNode(self.__root)
            elif ((__deletable_node.left is not None) 
                and (__deletable_node.right is not None)):
                self.__deleteInternalNode(__deletable_node)
            else:
                self.__deleteNode(__deletable_node)

            self.__size -= 1

            if __parent is not None:
                self.__rotate(__parent, True)

            return True

    # Deletes a node internal to the tree (no leaf) by swapping it with the
    # node with a maximum value but still below the value of the {@code node}.
    #
    # @param node
    #          The internal node to delete which should not be {@code null}.
    def __deleteInternalNode(self, node):
        assert (node is not None), "Null node passed."

        __max_of_min = self.__findMaxOfMinNode(node)

        if __max_of_min is None:
            return

        node.value = __max_of_min.value
        self.__deleteNode(__max_of_min)

    # Deletes a {@code node} from the tree, mainly leaf nodes.
    #
    # @param node
    #          The node to delete, which should not be {@code null}.
    def __deleteNode(self, node):
        assert (node is not None), "Null node passed."

        __parent = node.parent
        __left   = node.left
        __right  = node.right
        __pright = __parent.right
        __right_child = __pright is not None and __pright.value == node.value

        # Please note that we need not to check if 'parent' is null here,
        # because this can only happen if 'node' is the root, but this special
        # case is already recognized in the methode 'remove'.

        if (node.left is None) and (node.right is None):
            if __right_child:
                __parent.right = None
            else:
                __parent.left = None
        elif node.left is not None:
            if __right_child:
                __parent.right = __left
            else:
                __parent.left = __left

            __left.parent = __parent
        else:
            if __right_child:
                __parent.left = __right
            else:
                __parent.right = __right

            __right.parent = __parent

        node.parent = None

    # Finds the node with the maximum value in the left sub tree of 
    # {@code node}.
    #
    # @param node
    #         The node that marks the root of the corresponding sub tree which
    #         should not be {@code null}.
    # @return Returns the node with the maximum value in the left sub tree
    #         of {@code node}.
    def __findMaxOfMinNode(self, node):
        assert (node is not None), "Null node passed."

        __left_child = node.left

        while __left_child.right is not None:
            __left_child = __left_child.right

        return __left_child

    # Checks if the tree is empty (if it has no elements).
    #
    # @return Returns either {@code true} if the tree is empty, {@code false}
    #         otherwise.
    def isEmpty(self):
        return self.__size == 0

    # Clears the tree.
    def clear(self):
        self.__root = None
        self.__size = 0

    # Returns a string representation of the entire tree.
    #
    # @return Returns a string representation of the entire tree.
    def __str__(self):
        if self.isEmpty():
            return "(empty tree)"
        else:
            return self.__root.__str__()

    # Implementation of a single node of an AVL tree.<p>
    # Such a node holds a single value and references to both child nodes and
    # to its parent node. Furthermore, such a node has a balance, that depends
    # on the nodes' height in the tree. This balance value is essential for
    # tree rotations.
    #
    # @author Thomas Lang
    # @version 1.0, 2015-07-28
    # @see AVLTree#rotate(Node, boolean)
    # @see#getBalance()
    class Node(object):
        
        # Creates a new Node with the passed {@code value}.
        #
        # @param value
        #          The value hold by the newly created node.
        def __init__(self, value):
            self.__value  = value
            self.__left   = None
            self.__right  = None
            self.__parent = None

        # Gets the value of this node
        #
        # @return Returns the value of this node.
        @property
        def value(self):
            return self.__value

        # Sets the value of this node to the passed one.
        #
        # @param value
        #          The new value of this node.
        @value.setter
        def value(self, value):
            self.__value = value

        # Gets the left child of this node.
        #
        # @return Returns the left child of this node.
        @property
        def left(self):
            return self.__left

        # Sets the left child of this node to the passed one.
        #
        # @param left
        #          The new left child of this node.
        @left.setter
        def left(self, left):
            self.__left = left

        # Gets the right child of this node.
        #
        # @return Returns the right child of this node.
        @property
        def right(self):
            return self.__right

        # Sets the right child of this node to the passed one.
        #
        # @param right
        #          The new right child of this node.
        @right.setter
        def right(self, right):
            self.__right = right

        # Gets the parent node of this node.
        #
        # @return Returns the parent node of this node.
        @property
        def parent(self):
            return self.__parent

        # Sets the parent node to the passed one.
        #
        # @param parent
        #          The new parent node of this node.
        @parent.setter
        def parent(self, parent):
            self.__parent = parent
            
        # Returns the <em>balance</em> of this node, which is simply the
        # height of its right sub tree minored by the height of the left
        # sub tree.<p>
        # This value is essential for tree rotations that will stabilize the
        # tree after every operation if necessary.
        #
        # @return Returns the balance of this node.
        # @see AVLTree#rotate(Node, boolean)
        @property
        def balance(self):

            if self.__left is None and self.__right is None:
                return 0

            __lbal = 0 if self.__left is None else self.__left.height
            __rbal = 0 if self.__right is None else self.__right.height
            return __rbal - __lbal

        # Returns the height of this node in the surrounding tree.
        #
        # @return Returns the height of this node in the surrounding tree.
        @property
        def height(self):

            if self.__left is None and self.__right is None:
                # Note that for calculating purposes the minimum height is 1.
                return 1

            __lh = 0 if self.__left is None else self.__left.height
            __rh = 0 if self.__right is None else self.__right.height
            return 1 + max(__lh, __rh)

        # Returns a string representation of this node by listing up the value
        # of this node. On the left and the right handside of this value all
        # children are listed up including their own values.
        #
        # @return Returns a string representation of this node.
        def __str__(self):
            return ''.join(self.__re())

        # Creates the string representation of this node as described in the
        # documentation of __str__.
        #
        # @return Returns a lit of string representations.
        def __re(self):
            __rep = []
            
            if self.__left is not None:
                __rep.append("(")
                __rep.append(self.__left.__str__())
                __rep.append(")")

            __rep.append('[')
            __rep.append(str(self.__value))
            __rep.append(']')

            if self.__right is not None:
                __rep.append('(')
                __rep.append(self.__right.__str__())
                __rep.append(')')

            return __rep

# Main (testing) function.
def main():
    print("Creating tree ... ", end="")
    tree = AVLTree()
    print("done.")

    print("Provocating rotation ... ", end="")
    tree.insert(-42)
    tree.insert(7)
    tree.insert(999)
    print("done.")
    print("Actual tree:")
    print()
    print(tree)

    print("Provocating second rotation ... ", end="")
    tree.insert(10)
    tree.insert(144)
    print("done.")
    print("Actual tree:")
    print()
    print(tree)

    print("Provocating third rotation and root changing ... ", end="")
    tree.insert(9)
    print("done.")
    print("Actual tree:")
    print()
    print(tree)
    print()

    print("Does the tree contain '999'? -> ", end="")
    print("true" if tree.contains(999) else "false")
    print()

    print("Deleting root ... ", end="")
    tree.remove(10)
    print("done.")
    print("Deleting nodes '-42', '7' ... ", end="")
    tree.remove(-42)
    tree.remove(7)
    print("done.")
    print()

    print("Does the tree contain '-42'? -> ", end="")
    print("true" if tree.contains(-42) else "false")
    print()

    print("Actual tree:")
    print()
    print(tree)
    print()
    
    print("Clearing tree ... ", end="")
    tree.clear()
    print("done.")
    print("Tree should be empty now: ", end="")
    print(tree.isEmpty())

if __name__ == '__main__':
    main()
