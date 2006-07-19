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

(defmacro with-new-grammar-environment (&body body)
  `(let ((*grammar-environment* (make-hash-table :test 'eq))
         (*grammar-next-id* -1))
    ,@body))

(defun init-env (terminals)
  "Populate table with TERMINALS.  Returns list of term objects and
first ID of non-terminals."
  (let ((terms (mapcar #'(lambda (term) (get-nterm term :is-term t))
                       terminals)))
    (values terms *grammar-next-id*)))

(defun get-nterm (name &key is-term)
  "If NAME is NIL, return NIL.  Otherwise look for NAME in
environment.  If found, return it; otherwise create new object."
  (if (null name)
      nil
      (let ((from-hash (gethash name *grammar-environment*)))
        (if from-hash
            from-hash
            (setf (gethash name *grammar-environment*)
                  (make-nterm :name name
                              :id (incf *grammar-next-id*)
                              :is-term is-term))))))

(defun process-rule (s-rule)
  "Process given rule and return RULE object."
  (destructuring-bind (left args &rest right) s-rule
    (let ((left-nterm (get-nterm left))
          (right-nterms (mapcar #'get-nterm right)))
      (let ((rule (apply #'make-rule
                         :left left-nterm
                         :right right-nterms
                         args)))
        (push rule
              (nterm-rules left-nterm))
        rule))))

(defun set-precedence-info (grammar)
  ;; Set terminals' precedence info
  (loop :for (prec . terms) :in (grammar-prec-info grammar)
        :for idx :from 0
        :do (dolist (term terms)
              (let ((nt (nterm-by-name term grammar)))
                (setf (prec-assoc nt) prec)
                (setf (prec-priority nt) idx))))
  ;; Set rules' precedence info
  (loop :for rule :in (grammar-rules grammar) :do
        (if (and (slot-boundp rule 'priority)
                 (symbolp (prec-priority rule)))
            (let ((term (nterm-by-name (prec-priority rule) grammar)))
              (setf (prec-assoc rule)    (prec-assoc term)
                    (prec-priority rule) (prec-priority term)))
            (let ((last-term (find-if #'term-p (rule-right rule)
                                   :from-end t)))
              (when last-term
                (setf (prec-assoc    rule) (prec-assoc    last-term)
                  (prec-priority rule) (prec-priority last-term)))))))

(defun generate-action--class (lhs class rule-info rhs)
  (let ((rev-arglist '())
        (ignored '())
        (m-o-args '()))
    
    ;; Gather information and create left side
    (let ((new-right
           (mapcar
            #'(lambda (item)
                (let ((arg (gensym)))
                  (push arg rev-arglist)
                  (if (and (consp item)
                           (eq :initarg (first item)))
                      (progn
                        (assert (cddr item) nil
                                "Slot clause is too short: ~S" item)
                        (assert (not (cdddr item)) nil
                                "Slot clause is too long: ~S" item)
                        (push arg m-o-args) ; argument
                        (push (list (quote quote)
                                    (second item)) ; initarg
                              m-o-args)
                        (third item))
                      (progn
                        (push arg ignored)
                        item))))
            rhs)))
      `(,lhs (:action #'(lambda (,@(nreverse rev-arglist))
                           (declare (ignore ,@ignored))
                           (make-instance ',class
                                          ,@m-o-args))
               ,@rule-info)
        ,@new-right))))

(defun generate-action--form (lhs form rule-info rhs)
  (let ((rev-arglist '()))
    ;; Gather information and create left side
    (let ((new-right
           (mapcar
            #'(lambda (item)
                (if (and (consp item)
                         (eq :var (first item)))
                    (progn
                      (assert (cddr item) nil
                              "Var clause is too short: ~S" item)
                      (assert (not (cdddr item)) nil
                              "Var clause is too long: ~S" item)
                      (push (second item) rev-arglist) ; var name
                      (third item))
                    (progn
                      (push (gensym) rev-arglist)
                      item)))
            rhs)))
      (let ((arglist (nreverse rev-arglist)))
        `(,lhs (:action #'(lambda (,@arglist)
                            (declare (ignorable ,@arglist))
                            ,form)
                ,@rule-info)
          ,@new-right)))))

(defun generate-action (rule)
  (destructuring-bind (lhs (&rest rule-info
                            &key (action nil action-p)
                                 (form  nil form-p)
                                 (class  nil class-p)
                                 &allow-other-keys)
                           &rest rhs)
      rule
    (declare (ignore action))
    ;; Sanity check
    (let ((count (count t (list action-p form-p class-p))))
      (assert (>= 1 count) nil
              "More than one action-related key is provided: ~S" rule-info)
      (assert (= 1 count) nil
              "Action-related keys are not provided: ~S~%One of ~{~S ~} is expected." rule-info '(:action :form :class)))
    (cond
      (class-p
       (generate-action--class lhs class rule-info rhs))
      (form-p
       (generate-action--form lhs form rule-info rhs))
      (action-p
       `(,lhs ,rule-info ,@rhs))
      (t
       (error "Can't happen: checked earlier.")))))


(defun parse-complex-form (form rule pos)
  (let ((generated-rules ())
        (generated-sym1 (gensym)))
    (ecase (first form)
      ((:* *)
       (push `(,generated-sym1 (:action #'(lambda (cdr &rest car) ; Twisted!
                                            (append (reverse car) cdr)))
               ,generated-sym1 ,@(rest form))
             generated-rules)
       (push `(,generated-sym1 (:action (constantly nil))
               ) ; empty
             generated-rules)
       (values generated-sym1 generated-rules 'common-lisp:reverse))
      ((:+ +)
       (push `(,generated-sym1 (:action #'(lambda (cdr &rest car) ; Twisted!
                                            (append (reverse car) cdr)))
               ,generated-sym1 ,@(rest form))
             generated-rules)
       (push `(,generated-sym1 (:action #'list)
               ,@(rest form))
             generated-rules)
       (values generated-sym1 generated-rules 'common-lisp:reverse))
      ((:or or)
       (let* ((rule-left (first rule))
              (rule-action (second rule))
              (rule-right (cddr rule))
              (new-form (second form))
              (new-rules
               (loop :for or-clause :in (cddr form)
                     :collect `(,rule-left ,rule-action
                                ,@(replace (copy-list rule-right)
                                           (list or-clause)
                                           :start1 pos :end1 (1+ pos))))))
         (if (consp new-form)
             (multiple-value-bind (new-form2 new-rules2 transform2)
                 (parse-complex-form new-form rule pos)
               (values new-form2
                       (nconc new-rules2 new-rules)
                       transform2))
             (values new-form
                     new-rules
                     nil))))
      ((:maybe)
       (push (if (cddr form)
                 `(,generated-sym1 (:action #'list)
                   ,@(rest form))
                 `(,generated-sym1 (:action #'identity)
                   ,(second form)))
             generated-rules)
       (push `(,generated-sym1 (:action (constantly nil)))
             generated-rules)
       (values generated-sym1 generated-rules nil))
      ((:list)
       (destructuring-bind (item delim) (rest form)
         ;; Variable names in lambda are meaningless, but from
         ;; COMMON-LISP package.  This avoids "package FUCC-GENERATOR
         ;; not found" when loading FASLs with debug info.
         (push `(,generated-sym1 (:action #'(lambda (list cons car)
                                              (declare (ignore cons))
                                              (cons car list)))
                 ,generated-sym1 ,delim ,item)
               generated-rules)
         (push `(,generated-sym1 (:action #'list)
                 ,item)
               generated-rules)
         (values generated-sym1 generated-rules 'common-lisp:reverse))))))

(defun apply-argument-transforms (transforms rule-params)
  (destructuring-bind (&key action &allow-other-keys) rule-params
    (assert action nil
            "CAN'T HAPPEN: :ACTION is not found in ~S" rule-params)
    ;; Analyze the action
    (loop :for tr :in transforms
          :for arg := (gensym)
          :collect arg :into new-arglist
          :collect (if tr
                       `(,tr ,arg)
                       arg)
          :into arguments
          :finally (return
                     (list*
                      :action
                      (if (and (eq 'function (first action))
                               (not
                                (and
                                 (consp (second action))
                                 (eq 'setf (first (second action))))))
                          ;; Function name or lambda expression
                          `(function
                            (lambda ,new-arglist
                             (,@(rest action)
                               ,@arguments)))
                          `(function
                            (lambda ,new-arglist
                             (funcall ,action ,@arguments))))
                      rule-params)))))

(defun expand-rules (rules)
  (mapcan
   #'(lambda (rule)
       (let ((more-rules ())
             (transforms nil))
         (let ((new-rhs
                (loop :for form :in (cddr rule)
                      :for pos :from 0
                      :if (consp form)
                      :collect
                      (multiple-value-bind (new-nterm new-rules transform)
                          (parse-complex-form form rule pos)
                        (push transform transforms)
                        (setf more-rules
                              (nconc more-rules
                                     (expand-rules new-rules)))
                        new-nterm)
                      :else
                      :do (push nil transforms)
                      :and :collect form
                      :end)))
           (setf transforms (nreverse transforms))
           (list*
            `(,(first rule)
              ,(if (some #'identity transforms)
                   (apply-argument-transforms transforms (second rule))
                   (second rule))
              ,@new-rhs)
            more-rules))))
   rules))

(defun parse-grammar (initial terminals rules &key prec-info)
  (push +EOF+ terminals)
  (setf rules
        (append rules
                (list (list +START+ '(:action (function identity))
                            initial))))
  (with-new-grammar-environment
    (multiple-value-bind (terms first-nterm-id)  (init-env terminals)
      (let* ((proc-rules (mapcar #'process-rule
                                 (expand-rules
                                  (mapcar #'generate-action rules))))
             (nterms (sort (loop :for nterm
                                 :being :each :hash-value :of *grammar-environment*
                                 :when (not (term-p nterm))
                                 :collect nterm)
                          #'<
                          :key #'nterm-id)))
        (let ((grammar (make-grammar :first-nterm-id first-nterm-id
                                     :rules proc-rules
                                     :terms terms
                                     :nterms nterms
                                     :precedence prec-info)))
          (set-precedence-info grammar)
          grammar)))))

(defun nterm<= (a b)
  (cond
    ((null a)
     t)
    ((null b)
     nil)
    (t
     (<= (nterm-id a)
         (nterm-id b)))))

(defun nterm-by-name (name grammar)
  "Return NTERM by NAME in given GRAMMAR"
  (gethash name (grammar-environment grammar)))

(defun renumber-rules (grammar)
  (loop :for rule :in (grammar-rules grammar)
        :for idx :from 0 :do
        (setf (rule-index rule) idx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  FIRST function
;;;;

(define-property nterm-first)

(defun calculate-first (grammar)
  "Calculate FIRST for every nterm of grammar."
  ;; Assign FIRST of terms to themselves
  (dolist (term (grammar-terms grammar))
    (setf (nterm-first term) (list term)))
  ;; Set FIRST of nterms to NIL initially
  (dolist (nterm (grammar-nterms grammar))
    (setf (nterm-first nterm) nil))
  ;; Calculate FIRST of nterms
  (let ((more-repeats t))
    (loop
     :while more-repeats :do
     (setf more-repeats nil)
     (dolist (nterm (grammar-nterms grammar))
       (let ((nt-first-orig (nterm-first nterm))
             (nt-first-more (reduce #'(lambda  (a b)
                                        (ounion a b :ordering #'nterm<=))
                                    (nterm-rules nterm)
                                    :initial-value nil
                                    :key #'(lambda (rule)
                                             (seq-first (rule-right rule))))))
         (let ((nt-first-new (ounion nt-first-orig nt-first-more
                                     :ordering #'nterm<=)))
           (when (not (equal nt-first-orig nt-first-new))
             (setf (nterm-first nterm) nt-first-new)
             (setf more-repeats t))))))))

(defun item-first (something)
  (if (null something)
      '(nil)
      (nterm-first something)))

(defun seq-first (seq)
  "FIRST of list of nterms"
  (cond
    ((null seq)
     '(nil))
    ((null (rest seq))
     (item-first (first seq)))
    (t
     (let ((elt-first  (item-first (first seq))))
       (if (null (first elt-first))
           (ounion (rest elt-first) (seq-first (rest seq)) :ordering #'nterm<=)
           elt-first)))))

(defun combine-first (set1 set2)
  "If (FIRST-SEQ A) is SET1, (FIRST-SEQ B) is SET2, this function returns
 (FIRST-SEQ (APPEND A B))"
  (if (null (first set1))
      (ounion (rest set1) set2 :ordering #'nterm<=)
      set1))

(defmacro combine-first-sets (set &rest other-sets)
  "This macro is similair to COMBINE-FIRST, but it accepts variable
number of argument and doesn't evaluate unused expressions"
  (if (null other-sets)
      set
      (let ((temp-var (gensym)))
        `(let ((,temp-var ,set))
          (if (and ,temp-var (null (first ,temp-var)))
              (ounion (rest ,temp-var) (combine-first-sets ,@other-sets)
                      :ordering #'nterm<=)
              ,temp-var)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Function FOLLOW
;;;;

(define-property nterm-follow)

(defun calculate-follow (grammar)
  (push (nterm-by-name +EOF+ grammar)
        (nterm-follow (nterm-by-name '%$*Start grammar)))
  (let ((more-repeats t))
    (loop :while more-repeats :do
          (setf more-repeats nil)
          (dolist (rule (grammar-rules grammar))
            (loop :for (nt . tail) :on (rule-right rule) :do
                  (unless (null nt)
                    (let* ((follow-orig (nterm-follow nt))
                           (first (seq-first tail))
                           (follow-add
                            (ounion (if (member 'nil first)
                                        (nterm-follow (rule-left rule))
                                        nil)
                                    (remove nil first)
                                    :ordering #'nterm<=))
                           (follow-new (ounion follow-orig follow-add
                                               :ordering #'nterm<=)))
                      (unless (equal follow-orig follow-new)
                        (setf (nterm-follow nt) follow-new)
                        (setf more-repeats t)))))))))

