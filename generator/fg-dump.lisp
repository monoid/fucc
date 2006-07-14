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

(defvar *counter*)
(defvar *action-forms*)

(defun get-uniq-id (form)
  (multiple-value-bind (value present-p) (gethash form *action-forms*)
    (if present-p
        value
        (setf (gethash form *action-forms*)
              (incf *counter*)))))

(defun dump-action-to-2d (action-table)
  (let ((array (make-array (array-dimensions action-table)
                           :initial-element nil))
        (*counter* -1)
        (*action-forms* (make-hash-table :test 'equal))
        (actions-var (gensym))
        (array-var (gensym))
        (result-var (gensym))
        (i-var (gensym))
        (j-var (gensym))
        (redux-var (gensym)))
    (dotimes (state (array-dimension action-table 0))
      (dotimes (term (array-dimension action-table 1))
        (let ((actions (aref action-table state term)))
          (assert (null (rest actions)) nil
                  "Unresolved conflicts found at (~S ~S): ~S"
                  term state actions)
          (setf (aref array state term)
           (ecase (car (first actions))
             ((:shift)
              (list :shift :new-state (cddr (first actions))))
             ((:reduce)
              (let ((rule (cadr (first actions))))
                (list :reduce
                 :term (nterm-id (rule-left rule))
                 :len (if (null (first (rule-right rule)))
                          0
                          (rule-length rule))
                 :function (get-uniq-id (rule-action rule)))))
             ((:accept)
              '(:accept))
             ((nil)
              '(:error)))))))
    `(let* ((,actions-var
            (make-array ,(1+ *counter*)
                        :initial-contents
                        (list
                         ,@(loop
                            :with tmp := (make-array (1+ *counter*)
                                                     :initial-element nil)
                            :for form :being :each :hash-key :of *action-forms*
                                      :using (:hash-value id)
                            :do (setf (aref tmp id)
                                      form)
                            :finally (return (coerce tmp 'list))))))
           (,array-var ,array)
           (,result-var (make-array ',(array-dimensions action-table)
                                    :initial-element nil)))
      (dotimes (,i-var ,(array-dimension action-table 0))
        (dotimes (,j-var ,(array-dimension action-table 1))
          (setf (aref ,result-var ,i-var ,j-var)
                (ecase (first (aref ,array-var ,i-var ,j-var))
                  ((:shift)
                   (apply #'fucc::make-shift-action
                          (rest (aref ,array-var ,i-var ,j-var))))
                  ((:reduce)
                   (let ((,redux-var (apply #'fucc::make-reduce-action
                                            (rest (aref ,array-var
                                                        ,i-var ,j-var)))))
                     (setf #1=(fucc::reduce-action-function ,redux-var)
                           (aref ,actions-var #1#))
                     ,redux-var))
                  ((:error)
                   (fucc::make-error-action))
                  ((:accept)
                   (fucc::make-accept-action))))))
      ,result-var)))

(defun dump-goto-to-2d (goto-table)
  (let ((goto-tbl (make-array (array-dimensions goto-table)
                              :initial-element nil)))
    (dotimes (nterm (array-dimension goto-table 0))
      (loop :for (old-state . new-state) :in (aref goto-table nterm) :do
            (setf (aref goto-tbl old-state nterm)
                  new-state)))
    goto-tbl))

;;; Simple 2d tables
(defun dump-to-2d-tables (action-table goto-table)
  `(list ,(dump-action-to-2d action-table)
    ,(dump-goto-to-2d goto-table)))

;;; Action table is 2d (state, term) , goto table is list of
;;; (old-state . new-state) for given nterm
(defun dump-to-2d-and-1d (action-table goto-table)
  `(list ,(dump-action-to-2d action-table)
    ,goto-table))

;;; For each state give list of terminals that do not lead to error state
(defun dump-valid-terminals (action-table grammar)
  (let* ((size (array-dimension action-table 0))
         (result (make-array size :initial-element nil)))
    (dotimes (state size)
      (loop :for term-id :from 0 :below (array-dimension action-table 1)
            :for term :in (grammar-terms grammar) :do
            (let ((actions (aref action-table state term-id)))
              (when actions
                (push (nterm-name term) (aref result state)))))
      ;; Just to preserve correct order of terminals for user
      ;; convenience
      (setf #1=(aref result state)
            (subst nil +EOF+ (nreverse #1#))))
    ;; Update table: equal lists should be same.  Perhaps, it may save
    ;; some bytes :)
    (let ((ht (make-hash-table :test 'equal :size size)))
      (dotimes (state size)
        (let* ((val1 #1#)
               (val2 (gethash val1 ht)))
          (if val2
              (setf #1# val2)
              (setf (gethash val1 ht)
                    val1)))))
    result))
