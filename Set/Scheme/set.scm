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

;;; Creates a new and empty set.
(define make-set
  (lambda()
    (list '())))

;;; Checks if the set is empty or not.
;;;
;;; @param set
;;;         The set to check.
;;; @return Returns either true if the set is empty or false otherwise.
(define is-empty?
  (lambda(set)
    (equal? '(()) set)))

;;; Checks if the set contains the passed value.
;;;
;;; @param set
;;;          Set that may contain the passed value.
;;; @param value
;;;          The value to check if included in set.
;;; @returns Returns either true if the set contains the value or false 
;;;          otherwise.
(define contains?
  (lambda(set value)
    (cond ((is-empty? set) #f)
          (else
           (let ((head (car (car set)))
                 (tail (list (cdr (car set)))))
             (if (eq? head value)
                 #t
                 (contains? tail value)))))))

;;; Adds the passed {@code element} to the set.
;;; 
;;; @param set
;;;         The set to insert to.
;;; @param value
;;;         The new element to add.
;;; @return {@code true} if the element was added successfully, 
;;;         {@code false} otherwise.
(define insert
  (lambda(set value)
    (if (contains? set value)
        #f
        (begin
          (set-car! set (cons value (car set)))
          #t))))

;;; Removes the passed {@code element} from the set if contained.
;;; 
;;; @param set
;;;         The set to remove from.
;;; @param value
;;;         The element to remove from the set.
;;; @return {@code true} if the element was removed successfully from the 
;;;         set, {@code false} otherwise.
(define delete
  (lambda(set value)
    (if (is-empty? set)
        #f
        (begin
          (set-car! set (filter (car set) (lambda(x) (not (equal? x value)))))
          #t))))

;;; Clears the entire set by removing all elements from it.
;;;
;;; @param set
;;;         The set to clear.
(define clear
  (lambda(set)
    (set-car! set '())))

;;; Determines the size of the passed set.
;;;
;;; @param set
;;;         The set whose size to determine.
;;; @return Returns the size of the passed set.
(define size
  (lambda(set)
    (length (car set))))

;;; Prints the set.
;;;
;;; @param set
;;;         The set to print.
(define print-set
  (lambda(set)
    (display (car set))))

(define span
  (lambda(lis pred)
    (cond ((null? lis) '(() ()) )
          ((pred (car lis))
           (let* ((xs (span (cdr lis) pred))
                  (ys (car xs))
                  (zs (car (cdr xs))))
             (list (cons (car lis) ys) zs)))
          (else (list () lis)))))

(define group
  (lambda(list)
    (cond ((null? list) '())
          (else
           (let* ((head (car list))
                  (tail (cdr list))
                  (prem (span tail (lambda(x) (eq? x head))))
                  (prefix (car prem))
                  (remainder (car (cdr prem))))
             (cons (cons head prefix) (group remainder)))))))

(define filter
  (lambda(lis pred)
    (cond ((null? lis) '())
          ((pred (car lis)) (cons (car lis) (filter (cdr lis) pred)))
          (else (filter (cdr lis) pred)))))

(define (split-by l p k)
  (let loop ((low '())
             (high '())
             (l l))
    (cond ((null? l)
           (k low high))
          ((p (car l))
           (loop low (cons (car l) high) (cdr l)))
          (else
           (loop (cons (car l) low) high (cdr l))))))
 
(define (sortb l)
  (if (null? l)
      '()
      (split-by (cdr l) 
                (lambda (x) (>= x (car l)))
                (lambda (low high)
                  (append (sortb low)
                          (list (car l))
                          (sortb high))))))

(define rmdups
  (lambda(set)
    (map #'car (group (sortb set)))))

;;; Creates a new set containing the union of the first and
;;; the passed set.<p>
;;; The union of two sets describes all elements that are either contained
;;; in the first or in the second set, or both.
;;; 
;;; @param set1
;;;         The first set for the union.
;;; @param set2
;;;         The second set for the union.
;;; @return The union of the calling and the passed set
(define union
  (lambda(set1 set2)
    (cond ((is-empty? set1) set2)
          ((is-empty? set2) set1)
          (else
           (list (rmdups (append (car set1) (car set2))))))))

;;; Creates a new set containing the intersection of the first
;;; and the passed set.<p>
;;; The intersection of two sets describes all elements that are both
;;; contained in the first and the second set.
;;; 
;;; @param set1
;;;         The first set for the intersection.
;;; @param set2
;;;         The second set for the intersection.
;;; @return The intersection of the calling and the passed set.
(define intersect
  (lambda(set1 set2)
    (cond ((is-empty? set1) (make-set))
          ((is-empty? set2) (make-set))
          (else
           (map #'car 
                (filter (group (sortb (append (car set1) (car set2)))) 
                        (lambda(l) (not (null? (cdr l))))))))))

;;; Creates a new set containing the set difference between the
;;; first and the second passed set.
;;; The set difference of two sets A and B describes all elements out of A
;;; that are <b>not</b> contained in B.
;;; 
;;; @param set1
;;;         The first set of the difference computation.
;;; @param set2
;;;         The second set that will be deleted from the calling one.
;;; @return The set difference of the calling one and the passed set.
(define difference
  (lambda(set1 set2)
    (cond ((is-empty? set1) (make-set))
          ((is-empty? set2) set1)
          (else (diff (car set1) (car set2))))))

;;; Recursive helper-function for 'difference'.
(define diff
  (lambda(list1 list2)
    (cond ((null? list1) '())
          ((null? list2) list1)
          ((not (contains? (list list2) (car list1)))
           (cons (car list1) (diff (cdr list1) list2)))
          (else (diff (cdr list1) list2)))))
