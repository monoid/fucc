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

(cl:in-package #:fucc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Conditions
;;;

(define-condition parse-error-condition ()
  ((token-id :initarg :token-id
             :reader error-token-id
             :documentation "ID of token that caused the error")
   (data :initarg :data
         :reader error-data
         :documentation "Semantic data associated with token"))
  (:documentation "General parsing error"))

(define-condition lr-parse-error-condition
    (parse-error-condition)
  ((config :initarg :config
           :reader error-config
           :documentation "Runtime configuration of parser"))
  (:documentation "LR-specific parser error"))

(define-condition parse-conflict-condition (parse-error-condition)
  ((possible-actions :initform nil
                     :initarg :actions
                     :type list
                     :documentation "List of conflicting actions"
                     :reader possible-actions))
  (:documentation "Unresolved conflict in parser"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Parser types
;;;

(deftype term (&optional max-term)
  (if max-term
      `(integer 0 ,max-term)
      'fixnum))

;;#-production
(deftype state-row ()
  'vector)

;;#-production
(defstruct (state)
  (id 0 :type unsigned-byte)
  (table (error "") :type state-row))

;;#-production
(defmethod cl:print-object ((obj state) stream)
  (format stream "#<state ~S>" (state-id obj)))

#|
 #+production
 (deftype state ()
   'vector)

 (declaim (inline state-table make-state))

 (defun state-table (state)
   state)

 (defun make-state (&key id table)
   (declare (ignore id))
   table)

 (define-compiler-macro make-state (&key id table)
   (declare (ignore id))
   table)
|#

#|
 ;;; Version for spaghetti-code only
 (defstruct (shift-action)
  (new-state (error "") :type state))
|#

(defstruct (shift-action)
  (new-state (error "")))

(defstruct (reduce-action)
  (term 0 :type term)
  (len 0 :type unsigned-byte)
  (function #'list :type (or fixnum function)))

(defstruct (error-action)
  )

(defstruct (accept-action)
  )

(defun action-type (action)
  "Action designator (implementation-dependent)"
  (type-of action))

(defstruct lr-config
  (state-stack (error ")") :type list)
  (term-stack nil :type list)
  (data-stack nil :type list))

(defmacro with-lr-config ((state-stack-var term-stack-var data-stack-var)
                                stack
                                &body body)
  "Components of stack returned by STACK expressions are bound to
variables named by symbols in STATE-STACK-VAR, TERM-STACK-VAR and
DATA-STACK-VAR.  These variables can be used in the BODY."
  (declare (type symbol state-stack-var term-stack-var data-stack-var)
           (type t stack))
  (let ((stack-var (gensym)))
    `(let ((,stack-var ,stack))
      (symbol-macrolet ((,state-stack-var
                         (lr-config-state-stack ,stack-var))
                        (,term-stack-var
                         (lr-config-term-stack ,stack-var))
                        (,data-stack-var
                         (lr-config-data-stack ,stack-var)))
          ,@body))))

(progn
  (defvar *next-token*)
  ;; We don't want assign value to *next-token*, so set docstring
  ;; separately
  (setf (documentation '*next-token* 'variable)
        "Next token returned by lexer;
accessible in reduction actions."))

(progn
  (defvar *next-data*)

  (setf (documentation '*next-token* 'variable)
        "Data associated with next token returned by lexer;
accessible in reduction actions."))
