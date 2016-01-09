;;;; Copyright (c) 2016, Thomas Lang. All rights reserved.
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

;;; Struct describing a very basic queue.
;;; A queue is basically a list that allows the insertion of elements at the one
;;; and the removal of elements at the other end only.
;;;
;;; In this specific implementation, elements can be added at the very rear of it
;;; and removal only takes place at the front.
(defstruct queue
  (elements NIL)
  ;; For efficiency purposes we save a pointer to the very last element of the
  ;; queue here.
  (qlast NIL)
)

;;; Checks if the queue is empty or not.
;;;
;;; @param queue
;;;         The queue to check.
;;; @return Returns either true if the queue is empty or false otherwise.
(defun is-empty? (queue)
  (= (length (queue-elements queue)) 0))

;;; Pushes a list of new elements into the queue. If only a single element should be
;;; inserted, one has to pass it in form of a one-element-list.
;;;
;;; @param queue
;;;         The queue to push onto.
;;; @param value
;;;         A list of elements to offer to this list.
(defun offer (queue value)
  (cond 
    ;; If there are no values passed - return.
    ((null value) NIL)
    
    ;; But if and the queue is still empty, use the passed element list and set the
    ;; pointer to the very last element of the queue.
    ((or (null (queue-elements queue)) (null (queue-qlast queue)))
         (setf 
           (queue-elements queue) (nconc (queue-elements queue) value)
           (queue-qlast queue) (last value)))
    
    ;; Otherwise, there are some elements in both lists. Then we append the new 
    ;; elements by setting it as the tail of the last element, which is indeed an 
    ;; empty list. After that, reset this pointer.
    (t
      (setf
        (cdr (queue-qlast queue)) value
        (queue-qlast queue) (last value))))
  T)

;;; Deletes and returns the very first element of the queue.
;;;
;;; @param queue
;;;         The queue whose top element to delete if possible.
;;; @return Returns the top element from the queue after deletion.
(defun poll (queue)
  (if (is-empty? queue) NIL
    (let ((p (peek queue)))
      (if (null p)
        NIL
        (progn
          (setf (queue-elements queue) (cdr (queue-elements queue)))
          (if (null (queue-elements queue)) (setf (queue-qlast queue) NIL))
          p)))))

;;; Returns the last element from the queue.
;;;
;;; @param queue
;;;         The queue whose top element to return.
;;; @return Returns the top element from the queue if possible.
(defun peek (queue)
  (if (is-empty? queue)
    NIL
    (car (queue-elements queue))))

;;; Pretty-prints the queue
;;; 
;;; @param queue
;;;         The queue to pretty-print.
(defun print-queue (queue)
  (format t "~a~%" (queue-elements queue)))

