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

(in-package #:fucc-generator)

(defmacro fucc:defparser (variable initial (&rest terminals) (&rest rules)
                          &key prec-info
                               (type :lalr)
                               lexer-options)
  (let ((grammar (parse-grammar initial terminals rules :prec-info prec-info))
        (%/value/-var (gensym))
        (mapping-var (gensym))
        (state-var (gensym))
        (nterm-var (gensym))
        (term-var (gensym))
        (parser-var (gensym))
        (goto-table-var (gensym))
        (new-state-var (gensym))
        (use-context-p (member :context lexer-options)))
    ;; Check parameters
    (dolist (option (set-difference lexer-options '(:context)))
      (warn "Unknown lexer option: ~S" option))
    ;; Calculate grammar element's properties
    (renumber-rules grammar)
    (calculate-first grammar)
    (calculate-follow grammar)
    ;; Warning about unproductive or unused nonterminals
    (let* ((unproductive (delete-unproductive-nterm-rules grammar))
           (unused (delete-unused-term-rules grammar)))
      (when unproductive
        (setf unproductive
              (sort unproductive #'nterm<=))
        (warn "Unproductive nonterminals:~%~{ ~S~}"
              unproductive))
      (when unused
        (setf unused (sort unused #'nterm<=))
        (warn "Unused (non)terminals:~%~{ ~S~}"
              unused))
      (when (or unproductive unused)
        ;; Recalculate grammar properties
        (loop :for idx :from 0
              :for term :in (grammar-terms grammar) :do
              (setf (nterm-id term) idx))
        (loop :for idx :from (length (grammar-terms grammar))
              :for nterm :in (grammar-nterms grammar) :do
              (setf (nterm-id nterm) idx))
        (renumber-rules grammar)
        (calculate-first grammar)
        (calculate-follow grammar)))
    (let ((items (ecase type
                   ((:lalr)
                    (items-lalr grammar))
                   ((:lr :lr1)
                    (items-lr1 grammar))
                   ((:lr0 :slr)
                    (items-lr0 grammar)))))
      (multiple-value-bind (action goto)
          (generate-tables grammar
                           items
                           (ecase type
                             ((:lalr)
                              #'reduce-set-lalr)
                             ((:lr :lr1)
                              #'reduce-set-lr1)
                             ((:lr0)
                              #'reduce-set-lr0)
                             ((:slr)
                              #'reduce-set-slr)))
        (dotimes (i (array-dimension action 0))
          (dotimes (j (array-dimension action 1))
            (when (and (aref action i j)
                       (rest (aref action i j)))
              (setf (aref action i j)
                    (list
                     (linearize-conflicts
                      (aref action i j)))))))
        `(defparameter ,variable
          (load-time-value
           (let ((,%/value/-var ,(dump-to-2d-and-1d action goto))
                 (,mapping-var (fucc::alist-to-hash-table
                           ',(list*
                              (cons nil 0)
                              (mapcar #'(lambda (term)
                                          (cons (nterm-name term)
                                                (nterm-id term)))
                                      (rest (grammar-terms grammar)))))))
             (list
              0                      ; TODO: mechanism-dependent value
              #'(lambda (,state-var ,term-var ,parser-var) ; TODO: ditto
                  (setf ,term-var (or (gethash ,term-var ,mapping-var)
                                      (and ,term-var
                                           ;; TODO: specific condition type
                                           (error "Unknown terminal ~S" ,term-var))
                                      0))
                  (aref (fourth ,parser-var)
                        ,state-var ,term-var))
              #'(lambda (,state-var ,nterm-var ,parser-var) ; TODO: ditto
                  (let* ((,goto-table-var (fifth ,parser-var))
                         (,new-state-var
                          (cdr (assoc ,state-var
                                      (aref ,goto-table-var
                                            (- ,nterm-var ,(length (grammar-terms grammar))))))))
                    (assert ,new-state-var)
                    ,new-state-var))
              (first ,%/value/-var)
              (second ,%/value/-var)
              ,(if use-context-p
                   (dump-valid-terminals action grammar)
                   nil)))))))))
