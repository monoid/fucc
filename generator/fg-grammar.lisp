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
  (let ((terms (mapcar #'(lambda (term) (get-nterm term :is-terminal t))
                       terminals)))
    (values terms (1+ *grammar-next-id*))))

(defun get-nterm (name &key is-terminal)
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
                              :is-terminal is-terminal))))))

(defun split-rule-form (rule-form)
  "Convert rule form (A -> x -> y) into list ((A x) (A y))"
  (destructuring-bind (left-nterminal delim &rest data) rule-form
    (let ((left-sides (list nil)))
      (dolist (token data)
        (if (eq delim token)
            (push () left-sides)
            (push token (first left-sides))))
      (mapcar #'(lambda (rest) (cons left-nterminal (nreverse rest)))
              (nreverse left-sides)))))

(defun process-rule (s-rule)
  "Process given rule and return RULE object."
  (destructuring-bind (left args main-action &rest right) s-rule
    (let ((left-nterm (get-nterm left))
          (right-nterms (mapcar #'get-nterm right)))
      (let ((rule (apply #'make-rule
                         :left left-nterm
                         :right right-nterms
                         :action main-action
                         args)))
        (push rule
              (nterm-rules left-nterm))
        rule))))

(defun set-precedence-info (grammar)
  ;; Set terminals' precedence info
  (loop :for (prec . terminals) :in (grammar-prec-info grammar)
        :for idx :from 0
        :do (dolist (terminal terminals)
              (let ((nt (nterm-by-name terminal grammar)))
                (setf (prec-assoc nt) prec)
                (setf (prec-priority nt) idx))))
  ;; Set rules' precedence info
  (loop :for rule :in (grammar-rules grammar) :do
        (if (and (slot-boundp rule 'priority)
                 (symbolp (prec-priority rule)))
            (let ((terminal (nterm-by-name (prec-priority rule) grammar)))
              (setf (prec-assoc rule)    (prec-assoc terminal)
                    (prec-priority rule) (prec-priority terminal)))
            (let ((last-terminal (find-if #'terminal-p (rule-right rule)
                                   :from-end t)))
              (when last-terminal
                (setf (prec-assoc    rule) (prec-assoc    last-terminal)
                  (prec-priority rule) (prec-priority last-terminal)))))))

(defun expand-action--do (form rev-var-list rest)
  `(:call (function (lambda ,(if (rest rest)
                                 '() ; Arglist of non-primary actions in LR.
                                 (reverse rev-var-list))
            (declare (ignorable ,@rev-var-list))
            ,@(rest form)))))

(defun expand-action--class (form rhs rest rev-var-list rev-initarg-list)
  (unless (null (rest rest))
    (error "~S form ~S must be last in the clause: ~S" :class form rhs))
  `(:call (function (lambda ,(reverse rev-var-list)
            (declare (ignorable ,@rev-var-list))
            (make-instance (quote ,(second form))
                           ,@(reverse rev-initarg-list))))))

(defparameter +complex-attribute-form+
  '(:do :class))

(defparameter +attribute-form+
  '(:do :class :call))

(defun complex-attribute-form-p (form)
  "True if form is non-primitive attribute form."
  (and (consp form)
       (member (first form) +complex-attribute-form+)))

(defun attribute-form-p (form)
  "True if form is any attribute form."
  (and (consp form)
       (member (first form) +attribute-form+)))

(defun expand-action (rule)
  (destructuring-bind (lhs &rest rhs) rule
    (let ((exp-rhs (expand-rhs rhs)))
      (let ((primary-action (second (first (last exp-rhs))))
            (new-rhs (nbutlast exp-rhs)))
        `(,lhs ,nil ,primary-action ,@new-rhs)))))
      
(defun expand-rhs (rhs)
  (let ((var-list '())
        (rev-initarg-list '()))
    (nconc
     (loop :for form :in rhs
        :for rest :on rhs
        :collect
        (cond
          ((complex-attribute-form-p form)
           (ecase (first form)
             ((:do)
              (expand-action--do form var-list rest))
             ((:class)
              (expand-action--class form rhs rest var-list rev-initarg-list))))
          ((consp form)
           (case (first form)
             ((:var)
              ;; TODO: ASSERT is not proper form here
              (assert (cddr form) nil
                      "Var clause is too short: ~S" form)
              (assert (not (cdddr form)) nil
                      "Var clause is too long: ~S" form)
              (push (second form) var-list)
              (third form))
             ((:initarg)
              ;; TODO: ASSERT is not proper form here
              (assert (cddr form) nil
                      "Intarg clause is too short: ~S" form)
              (assert (not (cdddr form)) nil
                      "Init-Env clause is too long: ~S" form)
              (let ((var (gensym)))
                (push (second form)
                      rev-initarg-list)
                (push var
                      rev-initarg-list)
                (push var var-list)
                (third form)))
             (t
              (let ((var (gensym)))
                (push var var-list)
                form))))
          (t
           (let ((var (gensym)))
             (push var var-list)
             form))))
     ;; Primary action
     (cond
       ((null rhs)
        '((:call (constantly nil))))
       ((not (attribute-form-p (first (last rhs))))
        (if (rest rhs)
            '((:call (function list)))
            '((:call (function identity)))))
       (t
        '())))))

(defparameter +complex-forms+
  '(:* * :+ + :maybe :list :call))

;;;  Transform complex forms like "(:+ a)" into 3 values:
;;;
;;; 1. symbol that is used in original rule as substitution of the
;;; form ("substitution nterm").  It may be fresh symbol or some old one.
;;;
;;; 2. Set of new rules.  May be empty.
;;;
;;; 3. Value transformer.  This is a symbol naming a function or
;;; lambda-expression (but not result of its evaluation).  Transformer
;;; is applied to value generated by substitution nterm.  It is handy
;;; for efficient implementation of list forms.
(defun expand-complex-form (form rule pos)
  (declare (ignore rule pos))
  (let ((generated-rules ())
        (generated-sym1 (gensym)))
    (ecase (first form)
      ((:* *)
       (push `(,generated-sym1 nil
               ,generated-sym1 ,@(rest form)
               #'(lambda (cdr &rest car) ; Twisted!
                          (append (reverse car) cdr)))
             generated-rules)
       (push `(,generated-sym1 nil 
               (constantly nil)
               ;; empty
               )
             generated-rules)
       (values generated-sym1 generated-rules 'common-lisp:reverse))
      ((:+ +)
       (push `(,generated-sym1 nil
               #'(lambda (cdr &rest car) ; Twisted!
                   (append (reverse car) cdr))
               ,generated-sym1 ,@(rest form))
             generated-rules)
       (push `(,generated-sym1 nil
               #'list
               ,@(rest form))
             generated-rules)
       (values generated-sym1 generated-rules 'common-lisp:reverse))
      ((:maybe)
       (push (if (cddr form) ; Nested form is a list
                 `(,generated-sym1 nil
                   #'list
                  ,@(rest form) )
                 `(,generated-sym1 nil
                   #'identity
                   ,(second form)))
             generated-rules)
       (push `(,generated-sym1 nil
               (constantly nil)
               ;; Empty
               )
             generated-rules)
       (values generated-sym1 generated-rules nil))
      ((:list)
       (destructuring-bind (item delim) (rest form)
         ;; Variable names in lambda are meaningless, but from
         ;; COMMON-LISP package.  This avoids "package FUCC-GENERATOR
         ;; not found" when loading FASLs with debug info.
         (push `(,generated-sym1 nil
                 #'(lambda (list cons car)
                            (declare (ignore cons))
                            (cons car list))
                 ,generated-sym1 ,delim ,item)
               generated-rules)
         (push `(,generated-sym1 nil
                 #'list
                 ,item)
               generated-rules)
         (values generated-sym1 generated-rules 'common-lisp:reverse)))
      ((:call)
       (push `(,generated-sym1 nil
               (:call ,(second form))
               ;; Empty
               )
             generated-rules)
       (values generated-sym1 generated-rules nil)))))

(defparameter +inlineable-forms+
  '(:or or))

;;; Inline complex form (like :or).  Returned values are same as of
;;; expand-complex-form.
(defun expand-inlinable-form (form rule pos)
  (let ((generated-sym1 (gensym)))
    (ecase (first form)
      ((:or or)
       (let ((short-subforms ()) ; One-element forms are just inlined.
             (long-subforms ())) ; Forms with two or more elements
                                 ; are implemented with fresh nterm.
         (dolist (subform (rest form))
           (cond
             ((atom subform)        ; Atomic form is short
              (push subform short-subforms))
             ((null (rest subform)) ; One-element list is short
              (push (first subform) short-subforms))
             (t                     ; Everything else is long subform
              (push subform long-subforms))))
         ;; Bind long subforms to one artificial nterm
         (when long-subforms
           (push generated-sym1 short-subforms))

         ;; First short subform is returned as substitution nterm.
         ;; Rest of short subforms are inserted into new rules:
         ;; subform replaces inlinable form in original rule.
         (let* ((rule-left (first rule))
                (rule-meta (second rule))
                (rule-action (third rule))
                (rule-right (cdddr rule))
                (new-form (first short-subforms))
                (new-rules
                 (loop :for or-clause :in (rest short-subforms)
                       :collect `(,rule-left ,rule-meta
                                   ,rule-action
                                   ;; Short subform replaces inlinable
                                   ;; form
                                  ,@(replace (copy-list rule-right)
                                             (list or-clause)
                                             :start1 pos :end1 (1+ pos))))))
           (dolist (subform long-subforms)
             (push `(,generated-sym1 nil
                     #'list
                     ,@subform)
                   new-rules))
           ;; First short subform happens to be complex form, it is
           ;; processed recursively.
           (if (consp new-form)
               (multiple-value-bind (new-form2 new-rules2 transform2)
                   (parse-complex-form new-form rule pos)
                 (values new-form2
                         (nconc new-rules2 new-rules)
                         transform2))
               (values new-form
                       new-rules
                       nil))))))))

(defun parse-complex-form (form rule pos)
  (cond
    ((member (first form)
             +complex-forms+)
     (expand-complex-form form rule pos))
    ((member (first form)
             +inlineable-forms+)
     (expand-inlinable-form form rule pos))))

(defun apply-argument-transforms-to-action (transforms action)
  "Apply TRANSFORMS to ACTION"
  ;; Create new action
  (loop :for tr :in transforms
     :for arg := (gensym)
     :collect arg :into new-arglist
     :collect (if tr
                  `(,tr ,arg)
                  arg)
     :into arguments
     :finally (return
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
                       (funcall ,action ,@arguments)))))))

(defun expand-rules* (expander prefix-set rules)
  "Apply EXPANDER to forms starting with atoms in PREFIX-SET to all RULES."
  (mapcan
   #'(lambda (rule)
       (let ((more-rules ())
             (transforms nil))
         (let* ((rhs-expand
                 ;; Calculate new rhs and collect transforms into TRANSFORMS
                 (loop :for form :in (cdddr rule)
                    :for pos :from 0
                    :if (and (consp form)
                             (member (first form)
                                     prefix-set
                                     :test #'eq))
                    :collect
                    (multiple-value-bind (new-nterm new-rules transform)
                        (funcall expander form rule pos)
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
               ,(second rule)
               ,(if (some #'identity transforms)
                    (apply-argument-transforms-to-action transforms (third rule))
                    (third rule))
               ,@rhs-expand)
            more-rules))))
   rules))

(defun expand-rules (rules)
  (expand-rules* #'expand-inlinable-form
                +inlineable-forms+
                (expand-rules* #'expand-complex-form
                               +complex-forms+
                               rules)))

(defun parse-grammar (initial terminals rules &key prec-info)
  ;; Add EOF mark
  (push +EOF+ terminals)
  ;; Add artifical start rule
  (setf rules
        (append rules
                (list `(,+START+ #:-> ,initial (:call (function identity))))))
  (with-new-grammar-environment
    (multiple-value-bind (terminals first-nterminal-id)
        (init-env terminals) ;; Terminals added here
      (let* ((proc-rules (mapcar #'process-rule
                                 (expand-rules
                                  (mapcar #'expand-action
                                          (mapcan #'split-rule-form rules)))))
             (nterminals (sort (loop :for nterm
                                 :being :each :hash-value :of *grammar-environment*
                                 :when (not (terminal-p nterm))
                                 :collect nterm)
                          #'<
                          :key #'nterm-id)))
        (let ((grammar (make-grammar :first-nterminal-id first-nterminal-id
                                     :rules proc-rules
                                     :terminals terminals
                                     :nterminals nterminals
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
  "Find NTERM by NAME in given GRAMMAR"
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
  ;; Assign FIRST of terminals to themselves
  (dolist (terminal (grammar-terminals grammar))
    (setf (nterm-first terminal) (list terminal)))
  ;; Set FIRST of nterminals to NIL initially
  (dolist (nterminal (grammar-nterminals grammar))
    (setf (nterm-first nterminal) nil))
  ;; Calculate FIRST of nterminals
  (let ((more-repeats t))
    (loop
     :while more-repeats :do
     (setf more-repeats nil)
     (dolist (nterminal (grammar-nterminals grammar))
       (let ((nt-first-orig (nterm-first nterminal))
             (nt-first-more (reduce #'(lambda  (a b)
                                        (ounion a b :ordering #'nterm<=))
                                    (nterm-rules nterminal)
                                    :initial-value nil
                                    :key #'(lambda (rule)
                                             (seq-first (rule-right rule))))))
         (let ((nt-first-new (ounion nt-first-orig nt-first-more
                                     :ordering #'nterm<=)))
           (when (not (equal nt-first-orig nt-first-new))
             (setf (nterm-first nterminal) nt-first-new)
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

