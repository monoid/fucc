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

(defstruct ll-rule
  (actions)
  (nterms))


(defun stack-popper (n)
  (lambda (data-stack action-stack)
    (pop-n-stack data-stack action-stack n)))

;;; TODO: There is similar function in LR parsing: nsplit-list
(defun pop-n-stack (data-stack action-stack n)
  (let ((result '()))
    (loop :repeat n :do
       (push (pop data-stack) result))
    (push result data-stack)
    ;; Recursive call to parent rule action
    (funcall (first action-stack) data-stack (rest action-stack))))

(defun identity-stack (data-stack action-stack)
  (values data-stack action-stack))

(defun rule->ll-rule (rule)
  (make-ll-rule :nterms (reverse (rule-right rule))
                :actions (nreverse
                          (loop
                             :for rest :on (rule-right rule)
                             :for len  :from 1
                             :collect (if (null (rest rest))
                                          (stack-popper len)
                                          #'identity-stack)))))

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
                (dolist (termnl-follow (rule-left rule))
                  ;; We use PUSHNEW here becase rule may be addet two
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

(defun convert-to-deterministic-ll-table (table)
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
                 (rule->ll-rule (first #1#))))))))
  table)

;;; TODO: Consider redesign.  Instead of stacks use stack of stacks,
;;; where each stack on stack corresponds to rule in process.
;;; Now this structure is kept in actions, perhaps, keeping it in CONS
;;; structure may be more optimal.
(defun parse-ll (lexer ll-table grammar)
  (let ((nterm-stack (list (first (grammar-nterminals grammar))
                           (first (grammar-terminals grammar))))
        (data-stack '())
        (action-stack (list (lambda (data-stack action-stack)
                              (declare (ignore action-stack))
                              (return-from parse-ll (first data-stack))))))
    (loop
       (multiple-value-bind (tid data) (funcall lexer)
         ;; Do all possible rule expansions.
         (loop :until (terminal-p (first nterm-stack)) :do
            (let* ((nonterm (pop nterm-stack))
                   (ll-rule (aref ll-table (nterminal-id nonterm grammar) tid)))
              (loop
                 :for nterm :in (ll-rule-nterms ll-rule) :do
                 (push nterm nterm-stack))
              (loop
                 :for action :in (ll-rule-actions ll-rule) :do
                 (push action action-stack))))
         
         ;; Now there is either terminal or EOF on stack
         (if (equal tid
                    ;; TODO: NTERM-ID is wrong here (EOF is NIL!)
                    (nterm-id (first nterm-stack)))
             (progn
               (pop nterm-stack)
               (push data data-stack)
               ;; If we have complete, non-local exit is performed from action
               (multiple-value-setq (data-stack action-stack)
                 (funcall (first action-stack) data-stack (rest action-stack))))
             (error "LL parse error: ~S expected, ~S found"
                    (first nterm-stack)
                    tid))))))
