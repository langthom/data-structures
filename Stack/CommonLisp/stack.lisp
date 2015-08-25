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

;;; Creates a new and empty stack
(defun make-stack ()
  (list '()))

;;; Checks if the stack is empty or not.
;;;
;;; @param stack
;;;         The stack to check.
;;; @return Returns either true if the stack is empty or false otherwise.
(defun is-empty? (stack)
  (equal '(()) stack))

;;; Pushes a new element onto the stack.
;;;
;;; @param stack
;;;         The stack to push onto.
;;; @param value
;;;         The value to push.
(defun push-stack (stack value)
  (push value (car stack)))

;;; Deletes and returns the top element of the stack.
;;;
;;; @param stack
;;;         The stack whose top element to delete if possible.
;;; @return Returns the top element from the stack after deletion.
(defun pop-stack (stack)
  (pop (car stack)))

;;; Returns the top element from the stack.
;;;
;;; @param stack
;;;         The stack whose top element to return.
;;; @return Returns the top element from the stack if possible.
(defun peek (stack)
  (if (is-empty? stack)
      '()
    (car (car stack))))

;;; Pretty-prints the stack
;;; 
;;; @param stack
;;;         The stack to pretty-print.
(defun print-stack (stack)
  (format t "~a~%" (car stack)))
