;;;; bintree.lisp --- Implementation of a binary search tree.
;;;;
;;;; (C) Thomas Lang, 2014
;;;;
;;;; -------------------------------------------------------------------------
;;;; Summary:
;;;; --------
;;;; This module contains the implementation of a typical binary search tree.
;;;; This is a tree form, where each Node can have a maximum of two child
;;;; Nodes. Furthermore, it follows the rule, that all Nodes with a value
;;;; less than the current one are LEFT from it, while all Nodes with a value
;;;; greater than the current one are RIGHT from it.
;;;; In this implementation, duplicate Nodes (Nodes with same values) 
;;;; are NOT ALLOWED.
;;;; -------------------------------------------------------------------------
;;;; License:
;;;; --------
;;;; YOU ARE FREE TO CHANGE, COPY, DISTRIBUTE OR USE IT IN ANY IMPLEMENTATION
;;;; YOU WANT. THERE IS NO WARRANTY FOR THE CORRECTNESS OF THE BELOW CODE AND
;;;; THE AUTHOR IS NOT LIABLE FOR ANY ERRORS.
;;;; -------------------------------------------------------------------------

;;; ----------------------- [PACKAGE DEFINITION SECTION] ---------------------
(defpackage "BINARY-SEARCH-TREE"
  (:nicknames "BINTREE")
  (:use "COMMON-LISP")
  (:shadow "DELETE")
  (:export 
   "NEW-NODE" 
   "NEW-BINTREE" 
   "INSERT" 
   "PRINT-TREE" 
   "PREORDER"
   "INORDER"
   "POSTORDER"
   )
;   "DELETE")    ; see below
)

(in-package "BINARY-SEARCH-TREE")

;;; ----------------------- [DATA STRUCTURE DEFINE SECTION] ------------------

;;; structs representing node and tree

(defstruct node
  "Represents a single tree node.
   Such a node saves a value and 
   references to its left and right
   children as well to its parent node."
  value
  left 
  right 
  parent 
)

(defstruct bintree
  "Represents a binary search tree.
   Such a tree has only one root node
   and saves the number of nodes saved
   in this structure."
  root
  size 
)

;;; constructor-functions
(defun new-node (element)
  "Function: new-node/1
   Purpose : Allocates a new Node.
   Args    : The value of the Node.
   Returns : The newly created Node."
  (setf node (make-node :value element))
  node)

(defun new-bintree ()
  "Function: new-bintree/0
   Purpose : Allocates a new tree.
   Args    : None
   Returns : The newly created tree."
  (setf tree (make-bintree :size 0))
  tree)


;;; predicates
(defun is-empty (tree)
  "Function: is-empty/1
   Purpose : Checks if the passed tree is empty.
   Args    : The tree to check.
   Returns : T if tree is empty, NIL otherwise"
  (cond ((not (bintree-p tree)) (error "Param not a tree."))
        ((null (bintree-root tree)) 'T)
        (t 'NIL)))


;;; inserting
;;;
;;; internal note for me only:
;;; setq -> set QUOTED -> have to be symbol
;;; setf -> set FIELD  -> setting fields of struct

(defun insert (tree element)
  "Function: insert/2
   Purpose : Inserts a new Node containing the value
             'element' to the tree by following the
             rules for a binary search tree.
   Args    : The tree and the value of the new Node.
   Returns : T if successful"
  (if (not (bintree-p tree)) (error "Param 1 no tree!"))
  (if (not (integerp element)) (error "Param 2 no integer!"))
  (setq node (new-node element))
  (if (is-empty tree)
      (setf (bintree-root tree) node)
      (insert-rec (bintree-root tree) node))
  'T)

(defun insert-rec (cur node)
  "Function: insert-rec/2
   Purpose : Inserts the new Node recursively into the tree.
   Args    : The current and the new Node.
   Returns : T if successful, NIL if Node already existant."
  (cond ((< (node-value node) (node-value cur))
         (if (null (node-left cur))
             (progn
               (setf (node-left cur) node)
               (setf (node-parent node) cur)
               'T)
             (insert-rec (node-left cur) node)))
        ((> (node-value node) (node-value cur))
         (if (null (node-right cur))
             (progn
               (setf (node-right cur) node)
               (setf (node-parent node) cur)
               'T)
             (insert-rec (node-right cur) node)))
        (t 'NIL)))


;;; traversing

(defun preorder (tree)
  "Function: preorder/1
   Purpose : Traverses the tree preorder way.
   Args    : The tree to traverse.
   Returns : T if successfull, NIL if tree was empty."
  (if (not (bintree-p tree)) (error "Param no tree!"))
  (if (not (is-empty tree))
      (preorder-rec (bintree-root tree))
      'NIL)
  'T)

(defun preorder-rec (node)
  "Function: preorder-rec/1
   Purpose : Helper function for preorder traversing.
   Args    : The current Node.
   Returns : T."
  (print (node-value node))
  (if (not (null (node-left node)))
      (preorder-rec (node-left node)))
  (if (not (null (node-right node)))
      (preorder-rec (node-right node)))
  'T)

(defun inorder (tree)
  "Function: inorder/1
   Purpose : Traverses the tree inorder way.
   Args    : The tree to traverse.
   Returns : T if successful, NIL on empty tree."
  (if (not (bintree-p tree)) (error "Param no tree!"))
  (if (not (is-empty tree))
      (inorder-rec (bintree-root tree))
      'NIL)
  'T)

(defun inorder-rec (node)
  "Function: inorder-rec/1
   Purpose : Helper function for inorder traversing.
   Args    : The current Node.
   Returns : T."
  (if (not (null (node-left node)))
      (inorder-rec (node-left node)))
  (print (node-value node))
  (if (not (null (node-right node)))
      (inorder-rec (node-right node)))
  'T)

(defun postorder (tree)
  "Function: postorder/1
   Purpose : Traverses the tree postorder way.
   Args    : The tree to traverse.
   Returns : T if successful, NIL on empty tree."
  (if (not (bintree-p tree)) (error "Param no tree!"))
  (if (not (is-empty tree))
      (postorder-rec (bintree-root tree))
      'NIL)
  'T)

(defun postorder-rec (node)
  "Function: postorder-rec/1
   Purpose : Helper function for postorder traversing.
   Args    : The current Node.
   Returns : T."
  (if (not (null (node-left node)))
      (postorder-rec (node-left node)))
  (if (not (null (node-right node)))
      (postorder-rec (node-right node)))
  (print (node-value node))
  'T)


;;; deleting


(defun delete (tree element)
  (if (is-empty tree) (error "Cannot delete from empty tree!"))
  (if (not (bintree-p tree)) (error "Param1 no tree!"))
  (if (not (integerp element)) (error "Param2 no integer!"))
;  (let ((n (find-node tree element))) ; causing type error
;     (if (not (null n))
;         (cond ((= (node-value n) (node-value (bintree-root tree)))
;                (delete-internal-node (bintree-root tree)))
;               ((and (not (null (node-left n))) (not (null (node-right n))))
;                (delete-internal-node n))
;               (t (delete-node n)))
;         (error "Element not found!"))
  (if (not (null (find-node tree element)))
      (cond ((= (node-value (find-node tree element)) (node-value (bintree-root tree)))
             (delete-internal-node (bintree-root tree)))
            ((and (not (null (node-left (find-node tree element)))) (not (null (node-right (find-node tree element)))))
             (delete-internal-node (find-node tree element)))
            (t (delete-node (find-node tree element))))
      (error "Element not found!"))
    
  'T)

(defun delete-internal-node (node)
;  (setq a (find-max-of-min-node node))
;  (if (not (null a))
;      (progn
;        (setf (node-value node) (node-value a))
;        (delete-node a))
;      (error "Element not found!"))
  (if (not (null (find-max-of-min-node node)))
      (progn
        (setf (node-value node) (node-value (find-max-of-min-node node)))
        (delete-node (find-max-of-min-node node)))
      (error "Element not found!"))
  'T)

(defun delete-node (node)
  (cond ((and (null (node-left node)) (null (node-right node)))
         (if (= (node-value (node-right (node-parent node))) (node-value node))
             (setf (node-right (node-parent node)) 'NIL)
             (setf (node-left (node-parent node)) 'NIL)))

        ((not (null (node-left node))) 
         (if (= (node-value (node-right (node-parent node))) (node-value node))
             (progn
               (setf (node-right (node-parent node)) (node-left node))
               (setf (node-parent (node-left node)) (node-parent node)))
             (progn
               (setf (node-left (node-parent node)) (node-right node))
               (setf (node-parent (node-right node)) (node-parent node)))))
;
; NOTE THAT THIS OUTCOMMENTED CODE MAKES A TYPE ERROR, BECAUSE LISP DOES
; NOT ALLOW TO ASSIGN ONE STRUCTURE TO ANOTHER
; BECAUSE OF THAT, I WILL NOT EXPORT THE WHOLE DELETE THING
;
#|
        (t
         (if (= (node-value (node-right (node-parent node))) (node-value node))
             (progn
               (setf (node-left (node-parent node)) (node-right node))
               (setf (node-parent (node-left node)) (node-parent node)))
             (progn
               (setf (node-right (node-parent node)) (node-right node))
               (setf (node-parent (node-right node)) (node-parent node)))))
|#
)
  (setf (node-parent node) 'NIL)
  'T)

(defun find-node (tree element)
  (if (is-empty tree) (error "No elements in empty tree!")
      (if (not (null (find-node-rec (bintree-root tree) element)))
          (find-node-rec (bintree-root tree) element)
          (error "Element not found!"))))

(defun find-node-rec (node element)
  (cond ((= (node-value node) element) node)
        ((< element (node-value node)) (find-node-rec (node-left node) element))
        (t (find-node-rec (node-right node) element))))

;(defun find-max-of-min-node (node)
;  (if (not (null (node-left node)))
;      (progn
;        (setf n (node-left node))
;        (loop while (not (null (node-right n))) do
;               (setf n (node-right n)))
;        n)))

(defun find-max-of-min-node (node)
  (if (not (null (node-left node)))
      (find-max-of-min-node-rec (node-left node)))
)

(defun find-max-of-min-node-rec (node)
  (if (null (node-right node)) node
  (find-max-of-min-node-rec (node-right node))))


;;; printing

(defun print-tree (tree)
  "Function: print-tree/1
   Purpose : Prints the passed tree to stdout.
   Args    : The tree to print.
   Returns : T if successful."
  (if (not (bintree-p tree)) (error "Param no tree!"))
  (if (is-empty tree)
      (format t "(empty tree)~%")
      (progn
        (format t "Root: ~D~%" (node-value (bintree-root tree)))
        (print-tree-rec (bintree-root tree))
        (terpri)))
  'T)

(defun print-tree-rec (node)
  "Function: print-tree-rec/1
   Purpose : Helper function for printing the tree recursively.
   Args    : The current node.
   Returns : T if successful."
  (if (not (null (node-left node))) (format t "Left: ~D~%" (node-value (node-left node))))
  (if (not (null (node-right node))) (format t "Right: ~D~%" (node-value (node-right node))))
  (if (not (null (node-left node))) (print-tree-rec (node-left node)))
  (if (not (null (node-right node))) (print-tree-rec (node-right node)))
  'T)


;;; --------------- [TESTING (MAIN) SECTION] ---------------------

(setf test (new-bintree))
(print "*** Creating new empty binary search tree ... done.")
(terpri)
(format t "   ### Tree is empty? -> ~A~%" (is-empty test))
(dolist (x '(7 42 4 12 0 999))
  (insert test x))
(print "*** Inserting values 7, 42, 4, 12, 0, and 999 ... done.")
(terpri)
(format t "   ### Tree is empty? -> ~A~%" (is-empty test))
(print "   ### Current tree: ")
(terpri)
(print-tree test)
(terpri)
(print "*** Traversing preorder: ")
(preorder test)
(print "*** Traversing inorder: ")
(inorder test)
(print "*** Traversing postorder: ")
(postorder test)
(terpri)
(terpri)
(delete test 7)
(print "*** Deleting element '7' ... done.")
(terpri)
(print "   ### Current tree: ")
(terpri)
(print-tree test)
(terpri)