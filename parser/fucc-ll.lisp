#|
 Copyright (c) 2007 Ivan Boldyrev
                                             
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

(cl:in-package #:fucc)

(defun ll-rule-actions (ll-rule)
  (cdr ll-rule))

(defun ll-rule-nterms (ll-rule)
  (car ll-rule))

(defun nt->negative (id)
  (- (1+ id)))

(defun negative->nt (ng)
  (1- (- ng)))

(defun ll-terminal-p (id)
  (not (minusp id)))

(defmacro rev-apply (n func list-exp)
  (let ((args (loop :repeat n :collect (gensym)))
        (rest-var (gensym)))
    `(destructuring-bind (,@args &rest ,rest-var) ,list-exp
       (values (funcall ,func ,@(reverse args))
               ,rest-var))))

(defmacro stack-popper (n)
  `(lambda (data-stack action-stack)
    (pop-n-stack data-stack action-stack ,n)))

(defmacro ll-final-action (len action)
  "Final ACTION (form that returns function) where rule length
is LEN (unsigned byte)"
  `(function
    (lambda (data-stack action-stack)
     (multiple-value-bind (val new-data-stack)
         (rev-apply ,len ,action data-stack)
       (push val new-data-stack)
       (funcall (first action-stack) new-data-stack (rest action-stack))))))

(defmacro ll-middle-action (len action)
  "Non-final ACTION (form that return function).  LEN is unsigned
byte that defines number of preceeding tokens."
  (declare (ignore len))
  `(function
    (lambda (data-stack action-stack)
     ,@(if action
           `((funcall ,action))
           ())
     (values data-stack action-stack))))

;;; TODO: There is similar function in LR parsing: nsplit-list
(defun pop-n-stack (data-stack action-stack n)
  (let ((result '()))
    ;; Reduce data into list
    (loop :repeat n :do
       (push (pop data-stack) result))
    ;; Put list on a stack
    (push result data-stack)
    ;; Recursive call to parent rule action
    (funcall (first action-stack) data-stack (rest action-stack))))

(defun parser-ll (lexer ll-data)
  (destructuring-bind (ll-table init eof token-map) ll-data
    (let ((nterm-stack (list init eof))
          (data-stack '())
          (action-stack (list (lambda (data-stack action-stack)
                                (declare (ignore action-stack))
                                (return-from parser-ll (first data-stack)))))
          (lexer-proc (lambda () (multiple-value-bind (token attr)
                                     (funcall lexer)
                                   (multiple-value-bind (id found-p)
                                       (gethash token token-map)
                                     (if found-p
                                         (values id attr)
                                         (error "Unknown token: ~S" token)))))))
      (loop
         (multiple-value-bind (tid data) (funcall lexer-proc)
           ;; Do all possible rule expansions.
           (loop :until (ll-terminal-p (first nterm-stack)) :do
              (let* ((nonterm (pop nterm-stack))
                     (ll-rule (aref ll-table (negative->nt nonterm) tid)))
                (when (null ll-rule)
                  (error "No rule for ~S at ~S found"
                         (negative->nt nonterm)
                         tid))
                (if (null (ll-rule-nterms ll-rule))
                    ;; Epsilon rule
                    ;; Do action without pushing it on action-stack
                    (multiple-value-setq (data-stack action-stack)
                      (funcall (ll-rule-actions ll-rule) data-stack
                               action-stack))
                    ;; Do ordinary expansion
                    (progn
                      (loop
                         :for nterm :in (ll-rule-nterms ll-rule) :do
                         (push nterm nterm-stack))
                      (loop
                         :for action :in (rest (ll-rule-actions ll-rule)) :do
                         (push action action-stack))
                      ;; Execute initial action
                      ;; TODO: are NILs are really OK here?  Initial
                      ;; rule does not modify stack.
                      (funcall (first (ll-rule-actions ll-rule)) nil nil)))))
           ;; Now there is either terminal or EOF on stack
           (if (equal tid
                      (first nterm-stack))
               (progn
                 (pop nterm-stack)
                 (push data data-stack)
                 ;; Do action
                 ;; If we have complete, non-local exit is performed from action
                 (multiple-value-setq (data-stack action-stack)
                   (funcall (first action-stack) data-stack (rest action-stack))))
               (error "LL parse error: ~S expected, ~S found"
                      (first nterm-stack)
                      tid)))))))
