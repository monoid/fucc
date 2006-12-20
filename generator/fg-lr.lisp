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

(defun closure-lr1 (set)
  (closure
   set
   #'(lambda (lrp)
       (let ((rule (lrpoint-rule lrp))
             (pos (lrpoint-pos lrp))
             (result nil))
         (when (< pos (rule-length rule))
           (let ((nterm (elt (rule-right rule) pos))
                 (first-set (seq-first
                             (append (nthcdr (1+ pos) (rule-right rule))
                                     (lrpoint-lahead lrp)))))
             (when (and nterm (not (terminal-p nterm)))
               (dolist (terminal first-set)
                 (when terminal
                   (pushnew (list nterm terminal)
                            result
                            :test #'equal))))))
         result))
   #'(lambda (cons)
       (destructuring-bind (nterminal . lahead) cons
         (mapcar #'(lambda (rule)
                     (make-lrpoint :rule rule :pos 0 :lahead lahead))
                 (nterm-rules nterminal))))
   #'lrpoint<=))

(defun goto-lr1 (set nterm)
  (closure-lr1
   (goto-nc set
            nterm
            #'(lambda (nterm rule pos)
                (eq nterm (elt (rule-right rule)
                               pos))))))

(defun items-lr1 (grammar)
  "Calculate LR(1) items for GRAMMAR."
  (items grammar
         #'closure-lr1
         #'goto-lr1
         (make-lrpoint :rule (first (last (grammar-rules grammar)))
                       :pos 0
                       :lahead (list (nterm-by-name +EOF+ grammar)))))

(defun reduce-set-lr1 (item grammar)
  (declare (ignore grammar))
  (mapcan #'(lambda (lrpoint)
              (if (reduction-lrpoint-p lrpoint)
                  (list (cons (lrpoint-rule lrpoint)
                              (lrpoint-lahead lrpoint)))
                  nil))
          (item-set item)))

