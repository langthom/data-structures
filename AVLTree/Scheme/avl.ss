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

;;;;---------------------------------------------------------------------------+
;;;; REALLY IMPORTANT NOTE:                                                    |
;;;;   FOR DISPLAYING A NODE OR A TREE, ONLY USE THE FUNCTIONS                 |
;;;;     o 'print-node' OR                                                     |
;;;;     o 'print-tree'                                                        |
;;;;   ONLY, OTHER (DEFAULT) DISPLAYING WILL LIKELY CAUSE AN INFINITE          |
;;;;   LOOP DUE TO SOME CYCLIC REFERENCES. HOWEVER, THIS WILL NOT              |
;;;;   HAPPEN IF YOU USE THE ABOVE METHODS ONLY.                               |
;;;;---------------------------------------------------------------------------+

;;; Indicator that nothing changed to the balance of the tree."
(define NO_CHANGE 0)
;;;  "Indicator that the tree is 'one more heavy' on the right side."
(define REBAL_ONE_POS 1)
;;;  "Indicator that the tree is 'two more heavy' on the right side."
(define REBAL_TWO_POS 2)
;;;  "Indicator that the tree is 'one more heavy' on the left side."
(define REBAL_ONE_NEG -1)
;;;  "Indicator that the tree is 'two more heavy' on the left side."
(define REBAL_TWO_NEG -2)

;;;------------------------------ [START: NODE] --------------------------------

;;; Implementation of a single node of an AVL tree.<p>
;;; Such a node holds a single value and references to both child nodes and
;;; to its parent node. Furthermore, such a node has a balance, that depends
;;; on the nodes' height in the tree. This balance value is essential for
;;; tree rotations.
;;; 
;;; @author Thomas Lang
;;; @version 1.0, 2015-08-23
;;; @see AVLTree#rotate(Node, boolean)
;;; @see #getBalance()
(define make-node
  (lambda(value)
    (vector value '() '() '())))

;;; Gets the value of the node.
;;;
;;; @param node
;;;         The node to get its value.
;;; @return Returns the value of the node.
(define value-ref
  (lambda(node)
    (vector-ref node 0)))

;;; Sets the value of the node.
;;;
;;; @param node
;;;         The node to get its value.
;;; @param value
;;;         The new value of the node.
(define value-set!
  (lambda(node value)
    (vector-set! node 0 value)))

;;; Gets the left child of the node.
;;;
;;; @param node
;;;         The node to get its left child.
;;; @return Returns the left child of the node.
(define left-ref
  (lambda(node)
    (vector-ref node 1)))

;;; Sets the left child of the node.
;;;
;;; @param node
;;;         The node to set the child reference.
;;; @param left
;;;         The new child reference.
(define left-set!
  (lambda(node left)
    (vector-set! node 1 left)))

;;; Gets the right pointer of the node.
;;;
;;; @param node
;;;         The node to get its right child.
;;; @return Returns the right child of the node.
(define right-ref
  (lambda(node)
    (vector-ref node 2)))

;;; Sets the right child of the node.
;;;
;;; @param node
;;;         The node to set the child reference.
;;; @param right
;;;         The new child reference.
(define right-set!
  (lambda(node right)
    (vector-set! node 2 right)))

;;; Gets the parent pointer of the node.
;;;
;;; @param node
;;;         The node to get its parent node.
;;; @return Returns the parent node of the node.
(define parent-ref
  (lambda(node)
    (vector-ref node 3)))

;;; Sets the parent node of the node.
;;;
;;; @param node
;;;         The node to set the child reference.
;;; @param parent
;;;         The new parent reference.
(define parent-set!
  (lambda(node parent)
    (vector-set! node 3 parent)))

;;; Returns the <em>balance</em> of this node, which is simply the
;;; height of its right sub tree minored by the height of the left
;;; sub tree.<p>
;;; This value is essential for tree rotations that will stabilize the
;;; tree after every operation if necessary.
;;;   
;;; @param n
;;;         The node whose balance should be computed.
;;; @return Returns the balance of this node.
;;; @see AVLTree#rotate(Node, boolean)
(define get-balance
  (lambda(node)
    (let ((rn (equal? '() (right-ref node)))
          (ln (equal? '() (left-ref node))))
      (if (and rn ln) 0
          (let ((rbal (if rn 0 (get-height (right-ref node))))
                (lbal (if ln 0 (get-height (left-ref node)))))
            (- rbal lbal))))))

;;; Returns the height of this node in the surrounding tree.
;;; 
;;; @param n
;;;         The node whose height should be computed.
;;; @return Returns the height of this node in the surrounding tree.
(define get-height
  (lambda(node)
    (let ((rn (equal? '() (right-ref node)))
          (ln (equal? '() (left-ref node))))
      ;; Note that for calculating purposes the minimum height in a
      ;; tree is 1, not 0.
      (if (and rn ln) 1
          (let ((rh (if rn 0 (get-height (right-ref node))))
                (lh (if ln 0 (get-height (left-ref node)))))
            (+ 1 (max rh lh)))))))

;;; Prints out the passed node including all children.
;;; 
;;; @param n
;;;         The node to print.
(define print-node
  (lambda(node)
    (if (not (equal? '() (left-ref node)))
        (begin
          (display "(")
          (print-node (left-ref node))
          (display ")")))
    (display "[")
    (display (value-ref node))
    (display "]")
    (if (not (equal? '() (right-ref node)))
        (begin
          (display "(")
          (print-node (right-ref node))
          (display ")")))))

;;;-------------------------------- [END: NODE] --------------------------------

;;;------------------------------- [START: AVL] --------------------------------

;;;  Implementation of an AVL tree.<p>
;;;  An AVL tree is a binary tree named after its inventors Georgi Maximowitsch 
;;;  <strong>A</strong>delson-<strong>V</strong>elski and Jewgeni Michailowitsch
;;;  <strong>L</strong>andis. This kind of tree is a so called 
;;;  <em>self-balancing</em> tree, that is always as balanced as possible, so
;;;  such a tree cannot get degenerated by insertion or deletions of nodes.
;;;  This in advance guarantees, that search operations, insertions, deletions
;;;  <em>always</em> run in <code>O(log n)</code> with <code>n</code> as the 
;;;  number of nodes in the tree.
;;; 
;;;  @author Thomas Lang
;;;  @version 1.0, 2015-08-23
;;;  @see <a href="https://en.wikipedia.org/wiki/AVL_tree">AVLs on Wikipedia</a>
(define make-avl
  (lambda()
    (list '())))

;;; Returns the root node of the tree.
;;;
;;; @param tree
;;;         The tree whose root to get.
;;; @return The root node of the tree.
(define get-root
  (lambda(tree)
    (car (car tree))))

;;; Checks if the tree is empty (if it has no elements).
;;;   
;;; @param tree
;;;         The tree to check if empty or not.
;;; @return Returns either {@code true} if the tree is empty, {@code false}
;;;         otherwise."
(define is-empty?
  (lambda(tree)
    (equal? '(()) tree)))

;;;  "Inserts a new node with a value of {@code value} into this AVL tree.<p>
;;;   The insertion itself is just as like the insertion into a normal binary
;;;   search tree, but after this was successfully performed, a tree rotation
;;;   is made if necessary, so the tree is in balance again.
;;;   
;;;   @param tree
;;;           The tree to insert into.
;;;   @param value
;;;           The value of the new node to insert.
;;;   @return Returns {@code true} if the insertion was successful, 
;;;           {@code false} otherwise.
;;;   @see rotate(Node<T>, boolean)"
(define insert
  (lambda(tree value)
    (let ((node (make-node value))
          (success #t))
      (if (is-empty? tree)
          (set-car! tree (cons node '()))
          (set! success (insert-rec (get-root tree) node)))
      (rotate-tree tree node #f)
      success)))

;;; Performs the recursive insertion into the tree.<p>
;;; This function works <em>exactly</em> like the insertion into a binary
;;; search tree and because of that, its runtime lies in {@code O(log n)} 
;;; where {@code n} is the number of elements in the tree.
;;;  
;;; @param current
;;;           The traversal node used for recursion, that should not be 
;;;           {@code null}.
;;; @param n
;;;           The new node to insert, that should not be {@code null}.
;;; @return Returns {@code true} if the insertion was successful,
;;;         {@code false} otherwise."
(define insert-rec
  (lambda(current node)
    (let ((newvalue (value-ref node))
          (curvalue (value-ref current)))
      (cond ((< newvalue curvalue)
             (if (equal? '() (left-ref current))
                 (begin
                   (left-set! current node)
                   (parent-set! node current)
                   #t)
                 (insert-rec (left-ref current) node)))
            ((> newvalue curvalue)
             (if (equal? '() (right-ref current))
                 (begin
                   (right-set! current node)
                   (parent-set! node current)
                   #t)
                 (insert-rec (right-ref current) node)))
            (else #f)))))

;;; Performs a tree rotation around the node {@code node}.<p>
;;; This method performs a tree rotation around the passed node if 
;;; necessary. A rotation must only be performed, if the balance of the
;;; node reaches a certain level. The exact behaviour is different, if 
;;; it's a rotation for an insertion or if it's a rotation after deleting 
;;; a node.:
;;;    
;;; <ul>
;;; <li>
;;; If the rotation will be made after an insertion:
;;;   <ol>
;;;     <li>If the balance is zero, nothing changed to the tree, so there
;;;         need not to be any rotation taken.</li>
;;;     <li>If the balance is &plusmn; 1, then we must check the balance
;;;         of the parent node.</li>
;;;     <li>If the balance is &plusmn; 2, then we perform a rotation
;;;         depending on this balance and the balance of the previous child.
;;;     </li>
;;;   </ol>
;;; </li>
;;; <li>
;;; If the rotation will be made after the deletion of a node:
;;;   <ol>
;;;     <li>If the balance was zero, we must check the balance of the parent
;;;         node.</li>
;;;     <li>If the balance was &plusmn; 1, then nothing changed on the 
;;;         balances of the tree, so no rotation will be performed.</li>
;;;     <li>If the balance was &plusmn; 2, then we perform a rotation 
;;;         depending on that balance and the one of the deleted node.</li>
;;;   </ol>
;;; </li>
;;; </ul>
;;;   
;;; Note that as a special property of the <em>AVL</em> tree, the balances 
;;; of every node <em>always</em> lies in the range of 
;;; {@code -1 &leq; x &leq; 1} with {@code x} denoting the balance.
;;;
;;; @param tree
;;;          The tree where the rotation takes place in.
;;; @param n
;;;          The node to rotate over, which should not be {@code null}.
;;; @param deletion
;;;          Indicator if this is a rotation after an insertion or after
;;;          a deletion of the node.
;;; @see <a href="https://en.wikipedia.org/wiki/AVL_tree#Insertion">
;;;      Insertions and Deletions in AVL trees on Wikipedia
;;;      </a>
(define rotate-tree
  (lambda(tree node deletion)
    (if (and
         (eq? (get-root tree) node)
         (equal? '() (left-ref node))
         (equal? '() (right-ref node)))
        #t
        ;; If we have the pure root node here, which does not have any
        ;; children in this pure case, we need not to rotate anything.

        ;; Note that if we do not start with the root here, the pointer to the
        ;; parent node of the passed one is always not null.
        (let* ((par (if deletion node (parent-ref node)))
               (balance (if (equal? '() par) 0 (get-balance par))))
          (cond ((= balance NO_CHANGE)
                 (if (and deletion (not (equal? '() (parent-ref par))))
                     (rotate-tree tree (parent-ref node) deletion)
                     T))
                ((or (= balance REBAL_ONE_NEG) (= balance REBAL_ONE_POS))
                 (if (and (not deletion) (not (equal? '() (parent-ref par))))
                     (rotate-tree tree (parent-ref node) deletion)
                     (if deletion T)))
                (else
                 (let ((lbal (if (equal? '() (left-ref par)) 
                                 0 
                                 (get-balance (left-ref par))))
                       (rbal (if (equal? '() (right-ref par)) 
                                 0 
                                 (get-balance (right-ref par)))))

               ;;Performing rotations depending on the balances of the nodes:
               ;;
               ;; (1) If the node parents balance is +2:
               ;;
               ;;   (1.1) If the balance of the right child is +1, we perform a 
               ;;         'left' rotation, so the right child will become the 
               ;;         parent of its former parent node.
               ;;
               ;;   (1.2) If the balance of the right child is -1, we perform a
               ;;         'right-left' rotation, what is basically a 'right'
               ;;         rotation on the right child followed by a 'left' 
               ;;         rotation on the parent node.
               ;;
               ;; (2) If the node parents balance is -2:
               ;;
               ;;   (2.1) If the balance of the left child is +1, we perform a
               ;;         'left-right' rotation, what is basically a 'left'
               ;;         rotation on the left child followed by a 'right'
               ;;         rotation on the parent node.
               ;;
               ;;   (2.2) If the balance of the left child is -1, we perform a
               ;;         'right' rotation, so the left child will become the
               ;;         parent of its former parent node.
                   (if (= balance REBAL_TWO_POS)
                       (if (= rbal REBAL_ONE_POS)
                           (rotate-left tree par)
                           (if (= rbal REBAL_ONE_NEG)
                               (begin
                                 (rotate-right tree (right-ref par))
                                 (rotate-left tree par))))
                       (if (= balance REBAL_TWO_NEG)
                           (if (= lbal REBAL_ONE_POS)
                               (begin
                                 (rotate-left tree (left-ref par))
                                 (rotate-right tree par)))
                           (if (= lbal REBAL_ONE_NEG)
                               (rotate-right tree par)))))))))
  #t
))

;;; Performs a simple 'left' rotation around the passed node.
;;;
;;; @param tree
;;;          The tree where the rotation takes place in.
;;; @param n
;;;          The node to rotate around, which should not be {@code null}.
(define rotate-left
  (lambda(tree node)
    (let* ((par (parent-ref node))
           (rig (right-ref node))
           (riglef (left-ref rig)))
      (begin
        (parent-set! node rig)
        (right-set! node riglef)
        (parent-set! rig par)
        (left-set! rig node)
        (if (not (equal? '() riglef))
            (parent-set! riglef node))
        (cond ((equal? '() par) (set-car! tree (cons rig '())))
              ((eq? node (left-ref par)) (left-set! par rig))
              (else (right-set! par rig)))))
  #t
))

;;; Performs a simple 'right' rotation around the passed node.
;;;   
;;; @param tree
;;;          The tree where the rotation takes place in.
;;; @param n
;;;          The node to rotate around, which should not be {@code null}.
(define rotate-right
  (lambda(tree node)
    (let* ((par (parent-ref node))
           (lef (left-ref node))
           (lefrig (right-ref lef)))
      (begin
        (parent-set! node lef)
        (left-set! node lefrig)
        (parent-set! lef par)
        (right-set! lef node)
        (if (not (equal? '() lefrig))
            (parent-set! lefrig node))
        (cond ((equal? '() par) (set-car! tree (cons lef '())))
              ((eq? node (right-ref par)) (right-set! par lef))
              (else (left-set! par lef)))))
  #t
))

;;; Retrieves the node with the passed {@code value} from the tree.
;;;
;;; @param tree
;;;         The tree to search for the passed value in.
;;; @param value
;;;         The value of the node to return.
;;; @return Returns the node with the passed {@code value} from the tree,
;;;         or {@code null} if either the tree is empty (has no elements) or
;;;         if there is no such node in the tree.
(define get-node
  (lambda(tree value)
    (if (is-empty? tree)
        #f
        (get-rec (get-root tree) value))))

;;; Retrieves the node with the passed {@code value} from the tree 
;;; recursively.
;;;
;;; @param current
;;;           The traversal node used for recursion, which should not be
;;;           {@code null}.
;;; @param value
;;;           The value of the node to retrieve.
;;; @return Returns either {@code true} if the node with the passed 
;;;          {@code value} was found successfully or {@code null} otherwise.
(define get-rec
  (lambda(current value)
    (let ((curvalue (value-ref current)))
      (cond ((equal? value curvalue) current)
            ((and (< value curvalue) (not (equal? '() (left-ref current))))
             (get-rec (left-ref current) value))
            ((and (> value curvalue) (not (equal? '() (right-ref current))))
             (get-rec (right-ref current) value))
            (else '())))))

;;; Checks if the tree contains a node with the value {@code value}.
;;;   
;;; @param tree
;;;         The tree that possibly contains the passed value.
;;; @param value
;;;         The value of the node to check.
;;; @return Returns either {@code true} if the tree contains such a node, or
;;;         {@code false} if not.
(define contains
  (lambda(tree value)
    (not (equal? '() (get-node tree value)))))

;;; Removes the node with the passed {@code value} from the tree if
;;; existent. As a speciality of an AVL tree, this operation also runs
;;; in {@code O(log n)} with {@code n} denoting the number of nodes in the
;;; tree. Furthermore, even after a deletion of a node the tree will be 
;;; rotated if necessary.
;;;
;;; @param tree
;;;         The tree to remove the value in.
;;; @param value
;;;         The value of the node to delete.
;;; @return Returns either {@code true} if the node was successfully deleted
;;;         or {@code false} otherwise.
(define remove-node
  (lambda(tree value)
    (if (is-empty? tree)
        #f
        (let ((delnode (get-node tree value)))
          (if (equal? '() delnode)
              #f ; No node with 'value' found.
              (let ((par (parent-ref delnode)))
                (begin
                  (cond ((equal? value (value-ref (get-root tree)))
                         (deleteInternalNode (get-root tree)))
                        ((and
                          (not (equal? '() (left-ref delnode)))
                          (not (equal? '() (right-ref delnode))))
                         (deleteInternalNode delnode))
                        (else (deleteNode delnode)))
                  (if (not (equal? '() par))
                      (rotate-tree tree par #t)))))))
  #t
))

;;; Finds the node with the maximum value in the left sub tree of 
;;; the passed node.
;;;
;;; @param n
;;;         The node that marks the root of the corresponding sub tree which
;;;         should not be {@code null}.
;;; @return Returns the node with the maximum value in the left sub tree
;;;         of the passed node.
(define find-max-of-min
  (lambda(node)
    (let ((leftchild (left-ref node)))
      (find-max-of-min-rec leftchild))))

;;; Finds the max-of-min node recursively.
;;;
;;; @param node
;;;         The node whose max-of-min node to find.
;;; @return Returns the max-of-min node of {@code node}.
(define find-max-of-min-rec
  (lambda(node)
      (if (equal? '() (right-ref node))
          node
          (find-max-of-min-rec (right-ref node)))))

;;; Deletes a node internal to the tree (no leaf) by swapping it with the
;;; node with a maximum value but still below the value of the node.
;;;
;;; @param n
;;;          The internal node to delete which should not be {@code null}.
(define deleteInternalNode
  (lambda(node)
    (let ((maxofmin (find-max-of-min node)))
      (if (equal? '() maxofmin)
          #f
          (begin
            (value-set! node (value-ref maxofmin))
            (deleteNode maxofmin))))
    #t
))

;;; Deletes a node from the tree, mainly leaf nodes.
;;;
;;; @param n
;;;          The node to delete, which should not be {@code null}.
(define deleteNode
  (lambda(node)
    (let* ((par (parent-ref node))
           (lef (left-ref node))
           (rig (right-ref node))
           (pri (right-ref par))
           (prinull (not (equal? '() pri)))
           (right-child (and prinull (equal? (value-ref pri) (value-ref node)))))
      ;; Please note that we need not to check if 'parent' is null here,
      ;; because this can only happen if 'node' is the root, but this special
      ;; case is already recognized in the methode 'remove'.
      (cond ((and (equal? '() (left-ref node)) (equal? '() (right-ref node)))
             (if right-child
                 (right-set! par '())
                 (left-set! par '())))
            ((not (equal? '() (left-ref node)))
             (begin
               (if right-child
                   (right-set! par lef)
                   (left-set! par lef))
               (parent-set! lef par)))
            (else
             (begin
               (if right-child
                   (left-set! par rig)
                   (right-set! par rig))
               (parent-set! rig par))))
      (parent-set! node '()))
    #t
))

;;; Clears the tree.
;;; 
;;; @param tree
;;;         The tree to clear.
(define clear-tree
  (lambda(tree)
    (set-car! tree '())))


;;; Prints out the passed tree.
;;;
;;; @param tree
;;;          The tree to print.
(define print-tree
  (lambda(tree)
    (if (is-empty? tree)
        (display "(empty tree)\n")
        (print-node (get-root tree)))))

;;;-------------------------------- [END: AVL] ---------------------------------

(define main
  (lambda()
    (let ((tree (make-avl)))
      (begin
        (display "Creating tree ... ")
        (display "done.\n")
         
        (display "Provocating rotation ... ")
        (insert tree -42)
        (insert tree 7)
        (insert tree 999)
        (display "done.\n")
      
        (display "Actual tree:\n")
        (print-tree tree)
        (display "\n")

        (display "Provocating second rotation ... ")
        (insert tree 10)
        (insert tree 144)
        (display "done.\n")

        (display "Actual tree:\n")
        (print-tree tree)
        (display "\n")

        (display "Provocating third rotation and root changing ... ")
        (insert tree 9)
        (display "done.\n")

        (display "Actual tree:\n")
        (print-tree tree)

        (display "\nDoes the tree contain '999'? -> ")
        (if (contains tree 999)
            (display "Yes.\n")
            (display "No.\n"))

        (display "Deleting root ... ")
        (remove-node tree 10)
        (display "done.\n")

        (display "Deleting nodes '-42', '7' ... ")
        (remove-node tree -42)
        (remove-node tree 7)
        (display "done.\n")

        (display "Does the tree contain '-42'? -> ")
        (if (contains tree -41)
            (display "Yes.\n")
            (display "No.\n"))

        (display "Actual tree:\n")
        (print-tree tree)

        (display "\nClearing tree ... ")
        (clear-tree tree)
        (display "done.\n")

        (display "Tree should be empty now: ")
        (if (is-empty? tree)
            (display "Yes.\n")
            (display "No.\n"))
        ))))

;;; Invoking the main function.
(main)
