#!/bin/bash

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

from enum import Enum

class RedBlackTree():
    """
    Implementation of a red-black tree.<p>
    A red-black tree is a binary search tree whose nodes can have colors (either
    red or black). This tree is self-balancing and guarantees that all important
    operations (like searching, inserting and removing values) run in 
    {@code O(log n)} where {@code n} denotes the number of elements in the tree.
    <p>
    This rebalancing is done by guaranteeing the following rules:
    <ul>
      <li>
       The root is always black.
     </li>
     <li>
       The leaf nodes (what are {@code null} pointers in reality) are black.
     </li>
     <li>
       If a node is red, both children are black.
     </li>
     <li>
       Every path from a node to its children contain the same amount of black 
       nodes.
     </li>
    </ul>

    @author Thomas Lang
    @version 1.0, 2015-08-25
    @see Node
    """
    
    class Color(Enum):
        """ Enum representing the two possible colors of a node. """
        RED = 0
        BLACK = 1

        def __str__(self):
            """Returns a string representation of this enum."""
            return str(self.name)

    class Node():
        """
        Implementation of a single node of a Red-black tree.<p>
        This is basically a simple node of any binary search tree but with the
        additional {@code color} attribute of type {@code Color}, what can be
        either {@code Color.RED} or {@code Color.BLACK}.
        
        @author Thomas Lang
        @version 2015-08-25
        @see Color
        """

        def __init__(self, value):
            """
            Creates a new red node encapsulating the passed {@code value}.
            
            @param value
                 The value encapsulated in this node.
            """
            self.color  = RedBlackTree.Color.RED
            self.value  = value
            self.left   = None
            self.right  = None
            self.parent = None

        def __str__(self):
            """
            Returns a string representation of a node including all of its
            children.
            
            @return Returns a string representation of a node.
            """
            __rep = []

            if self.left is not None:
                __rep.append("(")
                __rep.append(self.left.__str__())
                __rep.append(")")

            __rep.append("[")
            __rep.append(str(self.color))
            __rep.append(": ")
            __rep.append(str(self.value))
            __rep.append("]")

            if self.right is not None:
                __rep.append("(")
                __rep.append(self.right.__str__())
                __rep.append(")")

            return ''.join(__rep)

    def __init__(self):
        """Creates a new and empty RedBlackTree."""
        self.__size = 0
        self.__root = None
    
    def isEmpty(self):
        """
        Checks if the tree is empty or not.
        
        @return Returns either {@code true} if the tree is empty or
                {@code false} otherwise.
        """
        return self.__size == 0

    def size(self):
        """
        Returns the number of elements stored in the tree.
        
        @return Returns the size of the tree.
        """
        return self.__size;

    def insert(self, value):
        """
        Inserts the passed value into the tree.
        
        @param value
                The value to insert into the tree which must not be 
                {@code null}.
        @return Returns either {@code true} if the insertion was successful or
                {@code false} otherwise.
        """
        if value is None:
            raise TypeError("Null value cannot be inserted.")

        __node = RedBlackTree.Node(value)
        __success = True;

        if self.isEmpty():
            __node.color = RedBlackTree.Color.BLACK
            self.__root = __node
        else:
            __success = self._insert(self.__root, __node)

        if __success:
            self.__size += 1

        return __success;

    def _insert(self, current, node):
        """
        Recursively traverses through the tree until the position for inserting
        is found. Then the node is inserted. If necessary, the tree is also 
        rebalanced starting from the new node.
        
        @param current
                The current traversal node which must not be {@code null}.
        @param node
                The node to insert into the tree which must not be {@code null}.
        @return Returns either {@code true} if the insertion was successful or
                {@code false} if the node already exists in the tree.
        @see #rebalance(Node<T>)
        """
        assert (current is not None), "Null traversing node passed."
        assert (node is not None), "Null node passed."

        __curvalue = current.value
        __newvalue = node.value
        __success = True

        if __newvalue < __curvalue:
            if current.left is None:
                current.left = node
                node.parent = current
                self._rebalance(node)
                __success = True
            else:
                __success = self._insert(current.left, node)
        elif __newvalue > __curvalue:
            if current.right is None:
                current.right = node
                node.parent = current
                self._rebalance(node)
                __success = True
            else:
                __success = self._insert(current.right, node)
        else:
            __success = False
        
        return __success;

    def _rebalance(self, node):
        """
        Rebalances the tree from {@code node} if necessary.
        
        @param node
                 The node to start rebalancing from.
        """
        assert (node is not None), "Null node passed."

        __parent = node.parent

        if __parent is None:
            # Case 1: The new node has no parent. This means that the new node
            # is the root and the root always must be black.
            node.color = RedBlackTree.Color.BLACK;
            return;

        if __parent.color == RedBlackTree.Color.BLACK:
            # Case 2: Per default every new node (including this one) are red.
            # When the color of the parent node is black, then the depth of
            # black nodes is still the same and we do not have to do anything.
            return;

        __grandparent = __parent.parent
        __uncle = __grandparent.right if __parent == __grandparent.left \
                                      else __grandparent.left

        if __uncle is not None and __uncle.color == RedBlackTree.Color.RED:
            # Case 3: Both the uncle and the parent nodes are red. Then we 
            # restore the tree by changing the below colors what makes the
            # tree be okay locally. But now, the grand parent will be
            # problematic, so we rebalance it.
            __parent.color = RedBlackTree.Color.BLACK
            __uncle.color = RedBlackTree.Color.BLACK
            __grandparent.color = RedBlackTree.Color.RED
            self._rebalance(__grandparent)
        else:
            # Case 4: The parent node and the node itself are red and the
            # path from the grand parent to the node forms a zig-zag line.
            # Then we perform a rotation and swap positions what will result
            # in a constellation useable for the fifth case.
            # The exact rotation depends on if the node was a left or a right
            # child.
            if node == __parent.right and __parent == __grandparent.left:
                self._rotateLeft(parent)
                node = node.left
            elif node == __parent.left and __parent == __grandparent.right:
                self._rotateRight(__parent)
                node = node.right

            # Case 5: From this position we restore the tree by swapping
            # colors and rotations around the grand parent, depending on if
            # the node was a left or a right child.
            __parent = node.parent
            __grandParent = __parent.parent

            __parent.color = RedBlackTree.Color.BLACK
            __grandParent.color = RedBlackTree.Color.RED

            if node == __parent.left and __parent == __grandParent.left:
                self._rotateRight(__grandParent)
            else:
                self._rotateLeft(__grandParent)

    def _rotateLeft(self, node):
        """
        Performs a single left rotation around the passed {@code node}.
        
        @param node
                 The node to rotate around which must not be {@code null}.
        """
        assert (node is not None), "Null node passed."

        __parent       = node.parent
        __right        = node.right
        __rightleft    = __right.left

        node.parent    = __right
        node.right     = __rightleft

        __right.parent = __parent
        __right.left   = node

        if __rightleft is not None:
            __rightleft.parent = node

        if __parent is None:
            self.__root = __right;
        elif __parent.left == node:
            __parent.left = __right;
        else: 
            __parent.right = __right;

    def _rotateRight(self, node):
        """
        Performs a single right rotation around the passed {@code node}.
         
        @param node
                 The node to rotate about which must not be {@code null}.
        """
        assert (node is not None), "Null node passed."

        __parent = node.parent
        __left = node.left
        __leftright = __left.right

        node.parent = __left
        node.left = __leftright

        __left.parent = __parent
        __left.right = node

        if __leftright is not None:
            __leftright.parent = node

        if __parent is None:
            self.__root = __left
        elif __parent.right == node:
            __parent.right = __left
        else:
            __parent.left = __left

    def contains(self, value):
        """
        Checks if the tree contains the passed {@code value}.
        
        @param value
                The value to search for.
        @return Returns either {@code true} if the tree contains the passed
                {@code value} or {@code false} otherwise.
        @see #get(T)
        """
        return self._get(value) is not None

    def _get(self, value):
        """
        Gets the node containing the passed {@code value}.
        
        @param value
                The value to search for.
        @return Returns either the node containing the passed {@code value} or
                {@code null} if the tree is empty or no node could be found.
        @see #get(Node<T>, T)
        """
        if self.isEmpty():
            return None
        else:
            return self.__get(self.__root, value)

    def __get(self, current, value):
        """
        Recursively gets the node containing the passed {@code value}.
        
        @param current
                The current traversal node, which must not be {@code null}.
        @param value
                The value to search for.
        @return Returns either the node containing the passed {@code value} or
                {@code null} if no such node could be found.
        """
        assert (current is not None), "Null traversing node."

        __curvalue = current.value

        if value == __curvalue:
            return current
        elif value < __curvalue and current.left is not None:
            return self.__get(current.left, value)
        elif value > __curvalue and current.right is not None:
            return self.__get(current.right, value)
        else:
            return None

    def remove(self, value):
        """
        Removes the passed {@code value} from this tree.<p>
        If necessary, the tree will be rebalanced after the removal. If the 
        passed {@code value} does not exist in the tree, this function does
        nothing and returns {@code false}.
        
        @param value
        @return Returns {@code true} if the removal was successful or 
                {@code false} otherwise.
        @see #deleteInternalNode(Node<T>)
        @see #deleteNode(Node<T>)
        """
        __node = self._get(value)

        if __node is None:
            return False

        __parent = __node.parent

        if value == self.__root.value:
            __rln = self.__root.left is None
            __rrn = self.__root.right is None
            if __rln and __rrn:
                self.__root = None
                return True
            elif not __rln and __rrn:
                __col = self.__root.color
                self.__root = self.__root.left
                self.__root.color = __col
            elif __rln and not __rrn:
                __col = self.__root.color
                self.__root = self.__root.right
                self.__root.color = __col
            else:
                self._deleteInternalNode(self.__root)
        elif __node.left is not None and __node.right is not None:
            self._deleteInternalNode(__node)
        else:
            self._deleteNode(__node)

        self.__size -= 1
        return True

    def _deleteInternalNode(self, node):
        """
        Deletes a node from the tree that has both two children.
        
        @param node
                 The node to delete which must not be {@code null}.
        @see #deleteNode(Node<T>)
        """
        assert (node is not None), "Null node passed."

        # The node is deleted by exchanging its value with the largest value
        # from the left sub tree and finally deleting the maximum node of the
        # left sub tree.
        __maxofmin = node.left

        while __maxofmin.right is not None:
            __maxofmin = __maxofmin.right

        node.value = __maxofmin.value
        self._deleteNode(__maxofmin)

    def _deleteNode(self, node):
        """
        Deletes a node from the tree that does not have two real children.
        The tree will be rebalanced if necessary.
        
        @param node
                 The node to delete which must not be {@code null}.
        @see #rebalanceAfterDeletion(Node<T>)
        """
        assert (node is not None), "Null node passed."

        __parent     = node.parent
        __left       = node.left
        __right      = node.right
        __pright     = __parent.right
        __rightchild = __pright is not None and __pright.value == node.value
        
        # Please note that we need not to check if 'parent' is null here,
        # because this can only happen if 'node' is the root, but this special
        # case is already recognized in the methode 'remove'.

        if node.left is None and node.right is None:
            if node.color == RedBlackTree.Color.BLACK:
                self._rebalanceAfterDeletion(node)

            if __rightchild:
                __parent.right = None
            else:
                __parent.left = None
        elif node.left is not None:
            if __rightchild:
                __parent.right = __left
            else:
                __parent.left = __left

            __left.parent = __parent
        else:
            if __rightchild:
                __parent.right = __right
                __parent.right.color = node.color
            else:
                __parent.left = __right
                __parent.left.color = node.color

            __right.parent = __parent

        node.parent = None
        node = None

    def _rebalanceAfterDeletion(self, node):
        """
        Rebalances the tree after a deletion.
        
        @param node
                 A child of the deleted node which must not be {@code null}.
        """
        assert (node is not None), "Null node passed."

        __parent = node.parent

        if __parent is None:
            # Case 1: Problematic node is root, no rotations to made.# 
            return

        __sibling = __parent.right if node == __parent.left else __parent.left

        if __sibling.color == RedBlackTree.Color.RED:
            # Case 2: The sibling of the node is red.
            # Then invert the colors of the parent and the sibling node
            # following by performing a left / right rotation around the
            # parent node depending on if the node was a left or a right
            # child.
            __parent.color = RedBlackTree.Color.RED
            __sibling.color = RedBlackTree.Color.BLACK

            if node == __parent.left:
                self._rotateLeft(parent)
            else:
                self._rotateRight(parent)

        __pcolor = __parent.color
        __scolor = __sibling.color
        __slcolor = RedBlackTree.Color.BLACK if __sibling.left is None \
                                else __sibling.left.color
        __srcolor = RedBlackTree.Color.BLACK if __sibling.right is None \
                                else __sibling.right.color

        if __pcolor == RedBlackTree.Color.BLACK \
                and __scolor == RedBlackTree.Color.BLACK \
                and __slcolor == RedBlackTree.Color.BLACK \
                and __srcolor == RedBlackTree.Color.BLACK:
            # Case 3: The parent, the sibling and both children of the sibling
            # are black. Then the sibling has the wrong color, so change it to
            # red. This may have corrupted any integraty conditions of the
            # parent node, so we have to rebalance the parent node.
            __sibling.color = RedBlackTree.Color.RED
            self._rebalanceAfterDeletion(__parent)
        elif __pcolor == RedBlackTree.Color.RED \
                and __scolor == RedBlackTree.Color.BLACK \
                and __slcolor == RedBlackTree.Color.BLACK \
                and __srcolor == RedBlackTree.Color.BLACK:
            # Case 4: The sibling and its both children are black but the 
            # parent is red. Then we can rebalance the tree by simply 
            # inverting the colors of the sibling and parent node.
            __sibling.color = RedBlackTree.Color.RED
            __parent.color = RedBlackTree.Color.BLACK
        else:
            # Case 5:
            # (a): Node is the left child and the sibling and the sibling's
            #      right child are black but the siblings left child is red.
            #      Then we change the colors of the sibling and it's left 
            #      child and perform a right rotation around the sibling.
            #      Then all paths have the same number of right nodes.
            #      After this, we immediately go to case 6.
            # (b): The same thing as in (a) but the other way (right child).
            if node == __parent.left \
                    and __scolor == RedBlackTree.Color.BLACK \
                    and __slcolor == RedBlackTree.Color.RED \
                    and __srcolor == RedBlackTree.Color.BLACK:
                __sibling.color = RedBlackTree.Color.RED
                __sibling.left.color = RedBlackTree.Color.BLACK
                self._rotateRight(sibling)
            elif node == __parent.right \
                    and __scolor == RedBlackTree.Color.BLACK \
                    and __slcolor == RedBlackTree.Color.BLACK \
                    and __srcolor == RedBlackTree.Color.RED:
                __sibling.color = RedBlackTree.Color.RED
                __sibling.right.color = RedBlackTree.Color.BLACK
                self._rotateLeft(sibling)

            # Case 6: The sibling is black, the right child of the sibling is
            # red and the node is the left child of it's parent.
            # Then we resolve this illegal state by changing the colors as
            # below. After this, we have to correct the now invalid paths
            # by rotating, depending on if the node was a left or a right 
            # child.
            __sibling.color = __parent.color
            __parent.color = RedBlackTree.Color.BLACK

            if node == __parent.left:
                __sibling.right.color = RedBlackTree.Color.BLACK;
                self._rotateLeft(__parent)
            else:
                __sibling.left.color = RedBlackTree.Color.BLACK;
                self._rotateRight(__parent)

    def clear(self):
        """Clears the tree."""
        self.__root = None
        self.__size = 0

    def __str__(self):
        """
        Returns a string representation of the entire tree.<p>
        This representation is either {@code (empty tree)} if the tree is empty
        or the representation of the root node.
        
        @return Returns a string representation of the entire tree.
        """
        if self.isEmpty():
            return "(empty tree)"
        else:
            return self.__root.__str__()

def main():
    """Main (testing) method."""
    print("Creating a new Red-black tree ... ", end="");
    test = RedBlackTree();
    print("done.");
    print("Inserting some values ... ", end="");
    test.insert(7);
    test.insert(8);
    test.insert(6);
    test.insert(12);
    test.insert(0);
    test.insert(9);
    test.insert(10);
    test.insert(-7);
    test.insert(999);
    print("done.");
    print("Tree now: " + str(test));

    print("Deleting a few values ... ", end="");
    test.remove(12);
    test.remove(0);
    test.remove(9);
    test.remove(7);
    test.remove(8);
    test.remove(10);
    test.remove(-7);
    test.remove(6);
    print("done.");
    print("Tree now: " + str(test));

    print("Clearing tree now ... ", end="");
    test.clear();
    print("done.");
    print("Tree now: " + str(test));

if __name__ == '__main__':
    main()
