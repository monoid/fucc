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

(defclass properties ()
  ((%properties :initform (make-hash-table :test 'eq) :type hash-table)))

(defmacro define-property (name)
  `(progn
    (defun ,name (obj)
      (gethash ',name (slot-value obj '%properties)))
    (defsetf ,name (obj) (value)
        `(setf (gethash ',',name (slot-value ,obj '%properties)) ,value))))

(defun prop-bound-p (obj name)
  (nth-value 1 (gethash name (slot-value obj '%properites))))

(defclass prec-mixin ()
  ((priority :accessor prec-priority
             :initarg :priority
             :initform -1
             :type number)
   (assoc :accessor prec-assoc
          :initarg :assoc
          :initform nil)))

(defmethod prec-priority ((number number))
  number)

;; TODO: remove action :initform and prec-mixin: unsuitable for LL
(defclass rule (properties prec-mixin)
  ((left :accessor rule-left
         :accessor rule-nterminal
         :initarg :left
         :documentation "Left-hand size: non-terminal")
   (right :accessor rule-right
          :accessor rule-production
          :initarg :right
          :documentation "Right-hand side: list of terms")
   (length :reader rule-length
           :documentation "Cached length of right-hand side")
   (index :accessor rule-index :initarg :index)
   (action :accessor rule-action
           :documentation "Action of the rule that creates rule's result"
           :initarg :action
           :initform (error "Action is not provided"))))

(defmethod initialize-instance :after ((rule rule) &rest arguments)
  (declare (ignore arguments))
  (when (slot-boundp rule 'right)
      (setf (slot-value rule 'length) (length (rule-right rule)))))

(defmethod (setf rule-right) :after (value (rule rule))
  (setf (slot-value rule 'length) (length value)))

(defun make-rule (&rest args)
  ;; We allow other keys here because :CLASS or :FORMS can be passed
  (apply #'make-instance 'rule :allow-other-keys t args))

(defun epsilon-rule-p (rule)
  "Check epsilon-rule."
  (let ((rhs (rule-right rule)))
    (or (null rhs)
        (and (null rhs) (null rhs)))))

(defclass nterm (properties prec-mixin)
  ((name :accessor nterm-name :initarg :name)
   (rules :accessor nterm-rules :initarg :rules :initform nil)
   (id :accessor nterm-id :initarg :id)
   (is-terminal :accessor terminal-p :initarg :is-terminal :initform nil)))

(declaim (inline nterminal-id))
(defun nterminal-id (terminal grammar)
  (- (nterm-id terminal) 1 (first-nterminal-id grammar)))

(defmethod nterm-name ((name null))
  nil)

(defmethod terminal-p ((term null))
  nil)

(defun make-nterm (&rest args)
  (apply #'make-instance 'nterm args))

(defvar *grammar-environment*)
(defvar *grammar-next-id*)

(defclass grammar ()
  ((nterminals :accessor grammar-nterminals :initarg :nterminals)
   (terminals :accessor grammar-terminals :initarg :terminals)
   (rules :accessor grammar-rules :initarg :rules)
   (environment :accessor grammar-environment
                :initarg :environment
                :initform *grammar-environment*)
   (first-nterminal-id :accessor first-nterminal-id
                   :initarg :first-nterminal-id
                   :initform *grammar-next-id*)
   (prec-info :accessor grammar-prec-info
               :initarg :precedence
               :initform nil
               :type list)))

(defun make-grammar (&rest args)
  (apply #'make-instance 'grammar args))

(defstruct lrpoint
  rule
  (pos 0 :type fixnum)
  lahead)

(defun advance-lrpoint (lrpoint)
  (make-lrpoint :rule (lrpoint-rule lrpoint)
                :pos (1+ (lrpoint-pos lrpoint))
                :lahead (lrpoint-lahead lrpoint)))

(defun nterm-at-pos (lrpoint)
  "Return nterm at position or NIL."
  (nth (lrpoint-pos lrpoint)
       (rule-right (lrpoint-rule lrpoint))))

(defun lrpoint<= (a b)
  (or (< (rule-index (lrpoint-rule a))
         (rule-index (lrpoint-rule b)))
      (and (eq (lrpoint-rule a)
               (lrpoint-rule b))
           (or (< (lrpoint-pos a)
                  (lrpoint-pos b))
               (and
                (= (lrpoint-pos a)
                   (lrpoint-pos b))
                (<= (nterm-id (first (lrpoint-lahead a)))
                    (nterm-id (first (lrpoint-lahead b)))))))))

(defun lrpoint= (a b)
  (and (eq (lrpoint-rule a)
           (lrpoint-rule b))
       (= (lrpoint-pos a)
          (lrpoint-pos b))
       (equal (lrpoint-lahead a)
              (lrpoint-lahead b))))

(defun lrpoint-hash (lrp)
  (logxor (rule-index (lrpoint-rule lrp))
          (lrpoint-pos lrp)
          (sxhash (lrpoint-lahead lrp))))

(defclass item (properties)
  ((set :reader item-set :initarg :set)
   (index :accessor item-index)
   (moves :accessor item-moves :initform nil)))

(defun make-item (&rest args)
  (apply #'make-instance 'item args))

(defun item= (a b)
  (funcall (list-cmp #'lrpoint=)
           (item-set a)
           (item-set b)))

(defun item-hash (item)
  (reduce #'logxor (item-set item) :key #'lrpoint-hash))

(defconstant +EOF+ '%$*EOF
  "Symbol used as end-of-file terminal.")

(defconstant +START+ '%$*Start
  "Symbol used as initial nonterminal.")

(defmethod print-object ((nterm nterm) output)
  (format output "#<~A :NAME ~S :ID ~S>"
          (if (terminal-p nterm)
              "TERM"
              "NTERM")
          (nterm-name nterm)
          (nterm-id nterm)))

(defmethod print-object ((rule rule) output)
  (format output "#<RULE ~S -> ~{~S ~} :ACTION ~S>"
          (nterm-name (rule-left rule))
          (mapcar #'nterm-name (rule-right rule))
          (rule-action rule)))

(defmethod print-object ((item item) output)
  (format output "<ITEM")
  (when (slot-boundp item 'index)
    (format output " :INDEX ~S" (item-index item)))
  (when (slot-boundp item 'set)
    (format output " :SET ~S" (item-set item)))
  (format output ">"))

