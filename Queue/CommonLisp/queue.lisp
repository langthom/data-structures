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

;;; Creates a new and empty queue
(defun make-queue ()
  (list '()))

;;; Checks if the queue is empty or not.
;;;
;;; @param queue
;;;         The queue to check.
;;; @return Returns either true if the queue is empty or false otherwise.
(defun is-empty? (queue)
  (equal '(()) queue))

;;; Pushes a new element onto the queue.
;;;
;;; @param queue
;;;         The queue to push onto.
;;; @param value
;;;         The value to push.
(defun offer (queue value)
  (push value (car queue)))

;;; Deletes and returns the last element of the queue.
;;;
;;; @param queue
;;;         The queue whose top element to delete if possible.
;;; @return Returns the top element from the queue after deletion.
(defun poll (queue)
  (if (is-empty? queue) '()
    (let ((p (peek queue)))
      (progn
        (setf (car queue) (reverse (cdr (reverse (car queue)))))
        p))))

;;; Returns the last element from the queue.
;;;
;;; @param queue
;;;         The queue whose top element to return.
;;; @return Returns the top element from the queue if possible.
(defun peek (queue)
  (if (is-empty? queue)
      '()
    (car (reverse (car queue)))))

;;; Pretty-prints the queue
;;; 
;;; @param queue
;;;         The queue to pretty-print.
(defun print-queue (queue)
  (format t "~a~%" (car queue)))
