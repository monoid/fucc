#|
 Copyright (c) 2006-2008 Ivan Boldyrev
                                             
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

;(declaim (optimize (speed 3) (debug 0) (safety 1)))


(declaim (inline nsplit-list))

(defun nsplit-list (list n)
  "Return two values: NREVERSEd prefix of LIST of length N and suffix.  LIST is
destructively modified."
  (declare (type list list)
           (type unsigned-byte n))
  (if (zerop n)
      (values nil list)
      (let* ((some-tail (nthcdr (1- n) list))
             (tail-list nil))
        (rotatef tail-list (cdr some-tail))
        (values (nreverse list) tail-list))))

;;;  Seems to be unreferenced
(defun longer-p (list len)
  "True, iff LIST is longher than LEN."
  (declare (type list list)
           (type unsigned-byte len))
  (nthcdr len list))


(defun parser-lr--shift! (config atom-id data state)
  (with-lr-config (state-stack term-stack data-stack) config
    (push atom-id term-stack)
    (push data data-stack)
    (push state state-stack))
  config)


(defun parser-lr--reduce! (config action parser *next-token* *next-data*)
  (declare (type lr-config config)
           (type reduce-action action)
           ;; Just to be sure:
           (special *next-token* *next-data*))
  (labels ((next-state (state term)
             (funcall (third parser) state term parser)))
    (with-lr-config (state-stack term-stack data-stack) config
      (let ((len (reduce-action-len action)))
        (multiple-value-bind (data rest-data)
            (nsplit-list data-stack len)
          (setf state-stack
                (nthcdr len state-stack))
          (let ((new-state (next-state (first state-stack)
                                       (reduce-action-term action))))
            (push new-state state-stack)
            (setf data-stack
                  (cons
                   (restart-case
                       (apply (reduce-action-function action) data)
                     (use-value (value)
                       value))
                   rest-data))
            (setf term-stack
                  (cons (reduce-action-term action)
                        (nthcdr len
                                term-stack)))
            config))))))


;; PARSER is a LIST: FIRST is initial state, SECOND is NEXT-ACTION
;; function, THIRD is NEXT-STATE, and rest is
;; implementation-dependent
(defun parser-lr (lexer parser)
  (declare (type function lexer))
  (let ((config (make-lr-config :state-stack
                                (list (first parser))))
        (atom-id)
        (data)
        (context-info (seventh parser)))
    (let ((lexer-fun (if context-info
                         #'(lambda (state)
                             (funcall lexer (aref context-info state)))
                         #'(lambda (state)
                             (declare (ignore state))
                             (funcall lexer)))))
      (declare (type function lexer-fun))
      (labels
          ((next-action (state term)
             (funcall (second parser) state term parser))
           (get-action (config atom-id)
             (next-action
              (first (lr-config-state-stack config))
              atom-id))
           (process-token (atom-id data)
             (do* ((action (get-action config atom-id)
                           (get-action config atom-id))
                   (action-type (action-type action)
                                (action-type action)))
                  ((not (member action-type
                                '(reduce-action error-action)))
                   nil)
               (restart-case
                   (if (eq action-type 'reduce-action)
                       (parser-lr--reduce! config action parser atom-id data) ; Reduction
                       ;;  Or error handling
                       (progn
                         ;; Try recover error without help.  Find
                         ;; error-handling state and generate CL:ERROR
                         ;; token.
                         (let ((states (lr-config-state-stack config))
                               (datas  (lr-config-data-stack  config)))
                           (loop :for sts :on states
                              :for state := (first sts)
                              :for action-type := (action-type (next-action state 'cl:error))
                              :for dats :on datas
                              :if (eq action-type 'shift-action) :do
                              ;; Error-handling state found; error
                              ;; recovery starts.  Unwind stack to the
                              ;; state.
                              (setf
                               (lr-config-state-stack config) sts
                               (lr-config-data-stack  config) dats)
                              (process-token 'cl:error nil)
                              ;; Look for possible sync tokens.  They
                              ;; are just that can be shifted.
                              (let ((tokens (cons nil
                                                  (loop
                                                     :for at-id :in (sixth parser)
                                                     :if (member (action-type (get-action config at-id))
                                                                 '(shift-action))
                                                     :collect at-id))))
                                (loop
                                   ;; Hey, found something!
                                   (when (member atom-id tokens)
                                     (if atom-id 
                                         (process-token atom-id data)
                                         (error 'lr-parse-error-condition
                                                :token-id atom-id
                                                :data data
                                                :config config))
                                     (return-from process-token))
                                   (multiple-value-setq (atom-id data) (funcall lexer-fun nil))))))
                         (error 'lr-parse-error-condition
                                :token-id atom-id
                                :data data
                                :config config)))
                 ;; Restarts
                 (skip-token ()
                   ;; Get next token
                   (return-from process-token))
                 (use-token (new-token-id new-data)
                   ;; Set this token instead of old
                   (setf atom-id new-token-id
                         data new-data))
                 (insert-token (new-token-id new-data)
                   ;; Process new token, then process old
                   (process-token new-token-id new-data))
                 (insert-token-list (token-cons-list)
                   ;; Process list of new tokens then process old
                   (loop :for (new-token-id . new-data) :in token-cons-list
                         :do (process-token new-token-id new-data)))))
             ;; Consume the token
             (let ((action (get-action config atom-id)))
               ;; Perform action
               (ecase (action-type action)
                 ;; Shift
                 (shift-action
                  (parser-lr--shift! config
                                     atom-id
                                     data
                                     (shift-action-new-state action)))
                 ;; Accept: parsing is complete
                 (accept-action
                  (return-from parser-lr
                    (first
                     (lr-config-data-stack config))))
                 ;; Reduction or error can't happen here: it was handled before
                 ((reduce-action error-action)
                  (error "Can't happen: ~A" (action-type action)))))))
        ;; Main loop; exit from the loop is performed with RETURN-FROM
        ;; somewhere else
        (loop
         ;; Get next token
         (multiple-value-setq (atom-id data)
           (funcall lexer-fun (first (lr-config-state-stack config))))
         (process-token atom-id data))))))
