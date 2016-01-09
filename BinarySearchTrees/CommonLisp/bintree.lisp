;;;; Copyright (c) 2015, Thomas Lang. All rights reserved.
;;;; 
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>

(defclass node ()
  ((value :accessor value 
          :initarg :value
          :documentation "Single value hold by this node.")
   (left :accessor left 
         :initarg :left 
         :initform nil
         :documentation "Pointer to this node's left child.")
   (right :accessor right 
          :initarg :right 
          :initform nil
          :documentation "Pointer to this node's right child.")
   (parent :accessor parent 
           :initarg :parent 
           :initform nil
           :documentation "Pointer to this node's parent node."))
  (:documentation 
   " Implementation of a single node of a binary search tree.<p>
    Such a node holds a single value and references to both child nodes and
    to its parent node.
   
    @author Thomas Lang
    @version 1.1, 2015-08-22")
)

(defmethod print-node ((n node))
  "Prints out the passed node including all children.

   @param n
           The node to print."
  (if (not (null (left n)))
      (progn
        (princ "(")
        (print-node (left n))
        (princ ")")))

  (princ "[")
  (princ (value n))
  (princ "]")

  (if (not (null (right n)))
      (progn
        (princ "(")
        (print-node (right n))
        (princ ")"))))



(defclass bintree ()
  ((size :accessor size 
         :initform 0 
         :initarg :size
         :documentation "The number of elements stored in the tree.")
   (root :accessor root 
         :initform nil 
         :initarg :root
         :documentation "The root node of the tree."))
  (:documentation
  "Implementation of a binary search tree.<p>
   This is a simple tree whose nodes always have two children that may be 
   {@code null}.
 
  @author Thomas Lang
  @version 1.1, 2015-08-22")
)


(defmethod is-empty ((tree bintree))
  " Checks if the tree is empty (if it has no elements).
   
    @param tree
            The tree to check if empty or not.
    @return Returns either {@code true} if the tree is empty, {@code false}
            otherwise."
  (null (root tree)))

(defmethod insert ((tree bintree) value)
  "Inserts a new node with a value of {@code value} into this binary search
   tree.<p> This obeys the rule that every left child has a smaller value 
   than the node, and every right child has a greater value than the node.
   
   @param tree
           The tree to insert into.
   @param value
           The value of the new node to insert.
   @return Returns {@code true} if the insertion was successful, 
           {@code false} otherwise."
  (let ((n (make-instance 'node :value value))
        (success T))
    (if (is-empty tree)
        (setf (root tree) n)
      (setf success (insert-rec (root tree) n)))
    (if success (incf (size tree)))
    success))

(defmethod insert-rec ((current node) (n node))
  " Performs the recursive insertion into the tree.<p>
   
    @param current
              The traversal node used for recursion, that should not be 
              {@code null}.
    @param n
              The new node to insert, that should not be {@code null}.
    @return Returns {@code true} if the insertion was successful,
            {@code false} otherwise."
  (let ((newvalue (value n))
        (curvalue (value current)))
    (cond ((< newvalue curvalue)
           (if (null (left current))
               (setf (left current) n (parent n) current)
             (insert-rec (left current) n)))
          ((> newvalue curvalue)
           (if (null (right current))
               (setf (right current) n (parent n) current)
             (insert-rec (right current) n)))
          (t nil))) ; no duplicates allowed
  T)

(defmethod get-node ((tree bintree) value)
  " Retrieves the node with the passed {@code value} from the tree.
   
    @param tree
            The tree to search for the passed value in.
    @param value
            The value of the node to return.
    @return Returns the node with the passed {@code value} from the tree,
            or {@code null} if either the tree is empty (has no elements) or
            if there is no such node in the tree."
  (if (is-empty tree)
      nil
    (get-rec (root tree) value)))

(defmethod get-rec ((current node) value)
  " Retrieves the node with the passed {@code value} from the tree 
    recursively.
   
    @param current
              The traversal node used for recursion, which should not be
              {@code null}.
    @param value
              The value of the node to retrieve.
    @return Returns either {@code true} if the node with the passed 
            {@code value} was found successfully or {@code null} otherwise."
  (let ((curvalue (value current)))
    (cond ((eql value curvalue) current) ; We found the node successfully!
          ((and (< value curvalue) (not (null (left current))))
           (get-rec (left current) value))
          ((and (> value curvalue) (not (null (right current))))
           (get-rec (right current) value))
          (T nil)))) ; No such node found.

(defmethod contains ((tree bintree) value)
  " Checks if the tree contains a node with the value {@code value}.
   
    @param tree
            The tree that possibly contains the passed value.
    @param value
            The value of the node to check.
    @return Returns either {@code true} if the tree contains such a node, or
            {@code false} if not."
  (not (null (get-node tree value))))

(defmethod remove-node ((tree bintree) value)
  " Removes the node with the passed {@code value} from the tree if
    existent.
   
    @param tree
            The tree to remove the value in.
    @param value
            The value of the node to delete.
    @return Returns either {@code true} if the node was successfully deleted
            or {@code false} otherwise."
  (if (is-empty tree)
      nil
    (let ((delnode (get-node tree value)))
      (if (null delnode)
          nil ; No node with 'value' found.
        (let ((par (parent delnode)))
            (cond ((eql value (value (root tree))) 
                   (deleteInternalNode (root tree)))
                  ((and 
                    (not (null (left delnode)))
                    (not (null (right delnode))))
                   (deleteInternalNode delnode))
                  (T (deleteNode delnode)))
            (decf (size tree))))))
  T
)

(defmethod find-max-of-min ((n node))
  " Finds the node with the maximum value in the left sub tree of 
    the passed node.
   
    @param n
            The node that marks the root of the corresponding sub tree which
            should not be {@code null}.
    @return Returns the node with the maximum value in the left sub tree
            of the passed node."
  (let ((leftchild (left n)))
    (loop while (not (null (right leftchild))) do
          (setf leftchild (right leftchild)))
    leftchild))

(defmethod deleteInternalNode ((n node))
  " Deletes a node internal to the tree (no leaf) by swapping it with the
    node with a maximum value but still below the value of the node.
   
    @param n
             The internal node to delete which should not be {@code null}."
  (let ((maxofmin (find-max-of-min n)))
    (if (null maxofmin)
        nil
      (progn
        (setf (value n) (value maxofmin))
        (deleteNode maxofmin))))
  T)

(defmethod deleteNode ((n node))
  " Deletes a node from the tree, mainly leaf nodes.
   
    @param n
             The node to delete, which should not be {@code null}."
  (let* ((par (parent n))
         (lef (left n))
         (rig (right n))
         (pri (right par))
         (right-child (and (not (null pri)) (eql (value pri) (value n)))))
    ;; Please note that we need not to check if 'parent' is null here,
    ;; because this can only happen if 'node' is the root, but this special
    ;; case is already recognized in the methode 'remove'.
    (cond ((and (null (left n)) (null (right n)))
           (if right-child
               (setf (right par) nil)
             (setf (left par) nil)))
          ((not (null (left n)))
           (progn
             (if right-child
                 (setf (right par) lef)
               (setf (left par) lef))
             (setf (parent lef) par)))
          (T (progn
               (if right-child
                   (setf (left par) rig)
                 (setf (right par) rig))
               (setf (parent rig) par))))
    (setf (parent n) nil))
  T
)

(defmethod clear ((tree bintree))
  "Clears the tree.
 
   @param tree
            The tree to clear."
  (setf (root tree) nil)
  (setf (size tree) 0)
  T)

(defmethod print-tree ((tree bintree))
  "Prints out the passed tree.

   @param tree
            The tree to print."
  (if (is-empty tree)
      (print "(empty tree)")
    (print-node (root tree)))
  T)


(defun main ()
  "Main (testing) function."
  (princ "Creating new tree ... ")
  (setf tree (make-instance 'bintree))
  (princ "done.")
  (terpri)

  (princ "Adding some values ... ")
  (insert tree 5)
  (insert tree 7)
  (insert tree 9)
  (insert tree 1)
  (insert tree 4)
  (insert tree 0)
  (insert tree 6)
  (insert tree 42)
  (insert tree 999)
  (princ "done.")
  (terpri)

  (princ "Actual tree:")
  (print-tree tree)
  (terpri)

  (princ "Actual tree size:")
  (print (size tree))
  (terpri)

  (princ "Deleting '5' ... ")
  (remove-node tree 5)
  (princ "done.")
  (terpri)

  (princ "Actual tree:")
  (print-tree tree)
  (terpri)

  (princ "Actual tree size:")
  (print (size tree))
  (terpri)

  (princ "Does the tree contain '5'? -> ")
  (if (contains tree 5)
      (princ "Yes.")
    (princ "No."))
  (terpri)

  (princ "Clearing tree ... ")
  (clear tree)
  (princ "done.")
  (terpri)

  (princ "Tree should be empty now ... ")
  (if (is-empty tree)
      (princ "Yes.")
    (princ "No."))
  (terpri)
)

;;; Invoking main function.
(main)
