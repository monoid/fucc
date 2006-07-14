#|
 Copyright (c) 2006 Ivan Boldyrev
                                             
 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and associated documentation files (the
 "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
                                             
 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.
                                             
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#

(cl:in-package #:fucc-generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ordered sets
;;;

(defun ounion (set1 set2 &key (ordering #'<=))
  "Union of two ordered sets: SET1 and SET2.
ORDERING is a reflexive ordering."
  (let ((result nil))
    (loop :while (and set1 set2) :do
          (let ((elt1 (first set1))
                (elt2 (first set2)))
            (let ((use1 (funcall ordering elt1 elt2))
                  (use2 (funcall ordering elt2 elt1)))
              (push (if use1 elt1 elt2)
                    result)
              (when use1
                (pop set1))
              (when use2
                (pop set2)))))
    (nreconc result
             (or set1 set2))))

(defun ointersection (set1 set2 &key (ordering #'<=))
  "Intersection of two ordered sets: SET1 and SET2.
ORDERING is a reflexive ordering."
  (let ((result nil))
    (loop :while (and set1 set2) :do
          (let ((elt1 (first set1))
                (elt2 (first set2)))
            (let ((skip1 (funcall ordering elt1 elt2))
                  (skip2 (funcall ordering elt2 elt1)))
              (when (and skip1 skip2)
                (push elt1 result))
              (when skip1
                (pop set1))
              (when skip2
                (pop set2)))))
    (nreverse result)))

;;; Fast'n'dirty recursive solution
(defun ocons (elt set &key (ordering #'<=))
  "Insert element into ordered set."
  (if (null set)
      (list elt)
      (let ((less (not (funcall ordering (first set) elt))))
        (cond
          (less
           (cons elt set))
          ((funcall ordering elt (first set))
           set)
          (t
           (let ((new-rest (ocons elt (rest set) :ordering ordering)))
             (if (eq new-rest (rest set))
                 set ; It may be happen if ELT is already in SET
                 (cons (first set) new-rest))))))))

(defun oset-ordering (ordering)
  "Return function that defines inducted ordering of ordered sets."
  (lambda (a b)
    (loop
     (cond
       ((null a)
        (return t))
       ((null b)
        (return nil))
       (t
        (if (not (funcall ordering (first a) (first b)))
            (return nil) ; (first a) > (first b)
            (progn
              (when (funcall ordering (first b) (first a))
                (pop b))                ; (first a) < (first b)
              (pop a))))))))

(define-modify-macro opush (elt &optional (ordering #'<=))
  (lambda (set elt ordering) (ocons elt set :ordering ordering))
  "Insert new element into ordered set.  Note that argument order is
different from PUSH.")

(defun list-cmp (predicate)
  "Return function that returns true if two lists are equal element
by element with PREDICATE."
  #'(lambda (list1 list2)
      (not (mismatch list1 list2 :test predicate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indirect hash-table
;;;

(defun iget (element hashtable hashfun test)
  (let ((hash-val (funcall hashfun element)))
    (or
     (find element
           (gethash hash-val
                    hashtable)
           :test test)
     (first (push element (gethash hash-val hashtable))))))

(defun itable-to-list (ht)
  (loop :for list :being :each :hash-value :of ht
        :nconc list))
