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

;;;; Grammar transformations and optimizations

(cl:in-package #:fucc-generator)

(define-property productive)

(defun delete-unproductive-nterm-rules (grammar)
  "Remove rules with unprductive nterms and nterms themself.
Return list of unprductive nterms."
  (let ((unproductive ()))
    ;; Terms are productive by definition
    (dolist (term (grammar-terms grammar))
      (setf (productive term) t))
    (dolist (nterm (grammar-nterms grammar))
      (unless (setf (productive nterm) (nterm-first nterm))
        (push nterm unproductive)))
    (dolist (rule (grammar-rules grammar))
      (setf (productive rule)
            (and (productive (rule-left rule))
                 (every #'productive (rule-right rule)))))
    ;; Prune grammar's nterm list and nterms' rules
    (setf (grammar-nterms grammar)
          (loop :for nterm :in (grammar-nterms grammar)
                :when (productive nterm)
                :collect nterm
                :do (setf (nterm-rules nterm)
                          (loop :for rule :in (nterm-rules nterm)
                                :when (productive rule)
                                :collect rule))))
    ;; Prune grammar's rule list
    (setf (grammar-rules grammar)
          (loop :for rule :in (grammar-rules grammar)
                :if (productive rule)
                :collect rule))
    unproductive))

(define-property nterm-used)

(defun delete-unused-term-rules (grammar)
  "Delete unused terms and nterms (i.e. they have no path from initial
terminal).  Return list of unused terms/nterms."
  (let ((unused ())
        (start (first (last (grammar-nterms grammar)))))
    ;; Find unused rules and rules
    ;; TODO: reimplement with bit vectors as sets
    (setf (nterm-used start) t)
    (let ((unprocessed (list start)))
      (loop :while unprocessed :do
            (let ((new-unprocessed ()))
              (dolist (nterm unprocessed)
                (dolist (rule (nterm-rules nterm))
                  (dolist (new-nterm (rule-right rule))
                    (unless (or (nterm-used new-nterm)
                                (member new-nterm new-unprocessed
                                        :test #'eq))
                      (push new-nterm new-unprocessed))))
                (setf (nterm-used nterm) t))
              (setf unprocessed new-unprocessed))))
    ;; Prune unused rules and rules
    (setf (grammar-nterms grammar)
          (loop :for nterm :in (grammar-nterms grammar)
                :if (nterm-used nterm)
                  :collect nterm
                :else
                  :do (push nterm unused)))
    ;; Update terms, preserving EOF term (which is always first)
    (let ((eof-term (first (grammar-terms grammar))))
      (setf (grammar-terms grammar)
            (cons eof-term
                  (loop :for term :in (rest (grammar-terms grammar))
                        :if (nterm-used term)
                          :collect term
                        :else
                          :do (push term unused)))))
    (setf (grammar-rules grammar)
          (loop :for rule :in (grammar-rules grammar)
                :when (nterm-used (rule-left rule))
                :collect rule))
    unused))

(defun add-epsilon (actions &optional (value-form nil))
  
  (mapcar
   #'(lambda (action)
       (destructuring-bind (lmbd (&rest arglist) (funcall act &rest realargs))
           action
         `(,lmbd ,arglist
           (,funcall ,act ,@(cons value-form realargs)))))
   actions))

(defun add-nterm (actions)
  (mapcar
   #'(lambda (action)
       (let ((var (gensym)))
         (destructuring-bind (lmbd (&rest arglist) (funcall rule &rest realargs))
             action
           `(,lmbd ,(cons var arglist)
             (,funcall ,rule ,@(cons var realargs))))))
   actions))

(define-property epsilon-action)

;; BUG: order of action execution is changed.  Example:
;;
;; A -> PRE-ACTION B POST-ACTION { $2 };
;; PRE-ACTION -> epsilon { (setup *global*) };
;; B -> ...  { (do-using *global*) };
;; POST-ACTION -> epsilon { (commit *global*) };
;;
;; After transformation it becomes:
;;
;; A -> B { ((lambda (a b c) $2) (setup *global*) $1 (commit *global*)) };
;; B -> ... { (do-using *global*) };
;;
;; Now B is reduced before *global* is set up, and it is wrong.
;;
;; It seems we _have_ to implement epsilon-rules properly.
;;
;;  Do other transformations change order of actions too?

(defun calculate-epsilon-actions (grammar)
  ;; Initial setup
  (dolist (rule (grammar-rules grammar))
    (when (epsilon-rule-p rule)
      (setf (epsilon-action (print (rule-left rule)))
            (list (rule-action rule)))))
  ;; Now propagate info
  (let ((initial-set (loop :for rule :in (grammar-rules grammar)
                           :for first-right := (seq-first (rule-right rule))
                           :when (and first-right (null (first first-right)))
                           :collect rule)))
    (loop :while initial-set :do
          (let ((new-set
                 (loop :for rule :in initial-set
                       :if (every #'epsilon-action (rule-right rule)) :do
                       ;; TODO: emit warning if there another action
                       ;; is already defined
                       (push (print`(lambda ()
                                (funcall
                                 ,(rule-action rule)
                                 ,@(mapcar #'(lambda (nterm)
                                               (first (epsilon-action nterm)))
                                           (rule-right rule)))))
                             (epsilon-action (rule-left rule)))
                       :else
                       :collect rule)))
            (setf initial-set (print new-set))))))

(defun remove-epsilon (rule epsilon-terms)
  (let ((eps-list (nreverse (mapcar #'(lambda (nterm)
                                        (or (assoc nterm epsilon-terms
                                                   :test #'eq)
                                            nterm))
                                    (rule-right rule)))))
    (if (and eps-list (every #'atom eps-list))
        (list rule)
        (let ((right-sides (list ()))
              (actions `((lambda ()
                           (funcall ,(rule-action rule))))))
          (loop :for eps-info :in eps-list :do
                (cond
                  ((atom eps-info)
                   ;; TODO: use MAP-INTO
                   (setf right-sides
                         (mapcar #'(lambda (list)
                                     (cons eps-info list))
                                 right-sides))
                   (setf actions
                         (add-nterm actions)))
                  ((cdr eps-info)
                   ;; Epsilon-only; do not add anyting to right sides
                   (setf actions
                         (add-epsilon actions
                                      (first
                                       (epsilon-action (car eps-info))))))
                  (t
                   (setf right-sides
                         (nconc right-sides
                                (mapcar #'(lambda (list)
                                            (cons (car eps-info) list))
                                        right-sides)))
                   (setf actions
                         (nconc (add-epsilon actions
                                             (first
                                              (epsilon-action (car eps-info))))
                                (add-nterm actions))))))
          ;; Now return list of RULE objects
          (mapcar #'(lambda (right action)
                      (make-rule :left (rule-left rule)
                                 :right right
                                 :action action))
                  right-sides
                  actions)))))

(defun remove-epsilon-rules (grammar)
  "Remove epsilon rules.  Actions are converted appropriately.
Return alist NTERM -> EPSILON-ONLY-P where EPSILON-ONLY-P is true iff
NTERM expands only to EPSILON or nothing.

This function may leave unused nterms.  Use DELETE-UNUSED-NTERM-RULES
to remove them"
  (let ((epsilon-terms (loop :for nterm :in (grammar-nterms grammar)
                             :for first-set := (nterm-first nterm)
                             :when (null (first first-set))
                             :collect (cons nterm (null (rest first-set))))))
    (dolist (nterm (grammar-nterms grammar))
      (setf (nterm-rules nterm)
            (loop :for rule :in (nterm-rules nterm)
                  :nconc (if (and (epsilon-rule-p rule)
                                  (not (eq +START+
                                           (nterm-name (rule-left rule)))))
                             nil
                             (remove-epsilon rule epsilon-terms)))))
    ;; TODO: is order of rules significant?
    ;; TODO: do this optmization before +START+ is added.  Then order
    ;;       is not significant.
    (setf (grammar-rules grammar)
          (loop :for nterm :in (grammar-nterms grammar)
                :append (nterm-rules nterm)))
    epsilon-terms))

