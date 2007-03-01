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

(defun filter-rule (rule-right grammar)
  "Transform nterm objects into their codes (negative for non-terminals)."
  (mapcar #'(lambda (nterm)
              (if (terminal-p nterm)
                  (nterm-id nterm)
                  (fucc::nt->negative (nterminal-id nterm grammar))))
          rule-right))

(defun rule->ll-rule (rule grammar)
  (cons
   (reverse
    (filter-rule (rule-right rule) grammar))
   (if (null (rule-right rule))
       `(fucc::ll-final-action 0 ,(rule-action rule))
       (list*
        'list
        ;; Initial action
        `(fucc::ll-middle-action 0 ,(rule-init-action rule))
        ;; Middle actions
        (nreverse
         (loop
            :for rest :on (rule-right rule)
            :for action :in (rule-middle-actions rule)
            :for len  :from 1
            :collect (if (null (rest rest))
                         `(fucc::ll-final-action ,len ,(rule-action rule))
                         `(fucc::ll-middle-action ,len ,action))))))))

(defun make-ll-table (grammar)
  "Generate LL table where each cell may contain list of possible
non-terminals.  You can later convert nondeterministic
tables (where some cell has more than one nonterminal) into
deterministic with some precedence rules or just reject such
tables."
  (let ((terminals-num (length (grammar-terminals grammar))))
    (let ((table (make-array (list (length (grammar-nterminals grammar))
                                   terminals-num)
                             :initial-element nil)))
      (dolist (rule (grammar-rules grammar))
        (let ((first-set (seq-first (rule-right rule))))
          (dolist (termnl first-set)
            (if (null termnl)
                ;; Epsilon derivation is possible.  Use FOLLOW set
                (dolist (termnl-follow (nterm-follow (rule-left rule)))
                  ;; We use PUSHNEW here becase rule may be added two
                  ;; times: as epsilon rule and as ordinary rule.  It
                  ;; is conflict, but it implicit one.  It is always
                  ;; demonstrated somewhere else explicitly.
                  (pushnew rule
                           (aref table
                                 (nterminal-id (rule-left rule) grammar)
                                 (nterm-id termnl-follow))))
                ;; Ordinary derivation is possible: just add rule to table
                (pushnew rule
                         (aref table
                               (nterminal-id (rule-left rule) grammar)
                               (nterm-id termnl)))))))
      table)))

(defun convert-to-deterministic-ll-table (table grammar)
  (dotimes (nterminal (array-dimension table 0))
    (dotimes (terminal (array-dimension table 1))
      (let ((value (aref table nterminal terminal)))
        (cond
          ((null value)
           nil)
          ((rest value)
           (error "Cannot resolve LL conflict: ~S" value))
          (t
           (setf #1=(aref table nterminal terminal)
                 (rule->ll-rule (first #1#) grammar)))))))
  table)

(defun det-ll-table->list-ll-parser-data (table)
  `(make-array
    ',(array-dimensions table)
    :initial-contents
    (list
     ,@(loop
          :for nterminal :from 0 :below (array-dimension table 0)
          :collect 
          (cons 'list
                (loop
                   :for terminal :from 0 :below (array-dimension table 1)
                   :for (rule . action) := (aref table nterminal terminal)
                   :if (aref table nterminal terminal)
                   :collect `(cons ',rule ,action)
                   :else
                   :collect nil))))))

(defun make-deterministic-ll-parser-data (table grammar)
  `(list
    ,(det-ll-table->list-ll-parser-data
       (convert-to-deterministic-ll-table table grammar))
    ,(fucc::nt->negative (nterminal-id (first (grammar-nterminals grammar))
                                       grammar))
    ,(nterm-id (first (grammar-terminals grammar)))
    (fucc::alist-to-hash-table
     ',(loop
          :for term :in (grammar-terminals grammar)
          :if (eq +EOF+ (nterm-name term))
          :collect (cons nil (nterm-id term))
          :else
          :collect (cons (nterm-name term) (nterm-id term))))))
