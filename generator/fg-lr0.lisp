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

(defun closure-lr0 (set)
  (closure set
           #'(lambda (lrp)
               (let ((lr-rule (lrpoint-rule lrp))
                     (lr-pos (lrpoint-pos lrp))
                     (result nil))
                 (when (< lr-pos (rule-length lr-rule))
                   (let ((nterm (elt (rule-right lr-rule) lr-pos)))
                     (when (and nterm (not (terminal-p nterm)))
                       (pushnew nterm result))))
                 result))
           #'(lambda (nterminal)
               (mapcar #'(lambda (rule)
                           (make-lrpoint :rule rule :pos 0))
                       (nterm-rules nterminal)))
           #'lrpoint<=))

(defun goto-lr0 (set nterminal)
  (closure-lr0
   (goto-nc set
            nterminal
            #'(lambda (nterminal rule pos)
                (eq nterminal (elt (rule-right rule)
                               pos))))))

(defun items-lr0 (grammar)
  "Calculate LR(0) items for the GRAMMAR."
  (items grammar
         #'closure-lr0
         #'goto-lr0
         (make-lrpoint :rule (first (last (grammar-rules grammar)))
                       :pos 0)))

(defun reduce-set-lr0 (item grammar)
  "Set rules that can be reduced in given item with LR0 aglorithm."
  (mapcan #'(lambda (lrpoint)
              (if (reduction-lrpoint-p lrpoint)
                  (mapcar #'(lambda (terminal)
                              (list (lrpoint-rule lrpoint) terminal))
                          (grammar-terminals grammar))
                  nil))
          (item-set item)))

(defun accept-set-slr-helper (lrpoint item grammar)
  (declare (ignore item))
  (if (eql +START+ (nterm-name (rule-left (lrpoint-rule lrpoint))))
      (list (list (lrpoint-rule lrpoint)
                  (nterm-by-name +EOF+ grammar)))
      (mapcar #'(lambda (terminal)
                  (list (lrpoint-rule lrpoint) terminal))
              (nterm-follow (rule-left (lrpoint-rule lrpoint))))))

(defun reduce-set-slr (item grammar)
  "Set rules that can be reduced in given item with LR0 aglorithm."
  (mapcan #'(lambda (lrpoint)
              (if (reduction-lrpoint-p lrpoint)
                  (accept-set-slr-helper lrpoint item grammar)
                  nil))
          (item-set item)))

