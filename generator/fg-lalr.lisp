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

(define-property item-kernel)

(defun kernel-lrpoint-p (lrpoint)
  (or (plusp (lrpoint-pos lrpoint))
      (and (eq +START+ (nterm-name (rule-left (lrpoint-rule lrpoint))))
           (zerop (lrpoint-pos lrpoint)))))

(defun kernel (item)
  (mapcan #'(lambda (lrpoint)
              (if (kernel-lrpoint-p lrpoint)
                  (list lrpoint)
                  nil))
          (if (listp item)
              item
              (item-set item))))

(defparameter +wildcard+
  (make-nterm :name 'no-such-symbol
              :id -1
              :is-terminal t)
  "Special wildcard terminal for lookahead tracing.")

(setf (nterm-first +wildcard+) (list +wildcard+))

(define-property channels)

(define-property spontaneous-laheads)

(defun simple-lrpoint (lrpoint item)
  (find-if #'(lambda (lrpt)
               (and (eq (lrpoint-rule lrpt)
                        (lrpoint-rule lrpoint))
                    (equal (lrpoint-pos lrpt)
                           (lrpoint-pos lrpoint))))
           (item-set item)))

(defun set-channel-info (from-item from-lrp to-item to-lrp)
  (pushnew (list* (simple-lrpoint from-lrp from-item)
                  to-item
                  (simple-lrpoint to-lrp to-item))
           (channels from-item)
           :test #'equal))

(defun set-spontaneous-lahead-info (lrpoint item)
  (let ((lrp (simple-lrpoint lrpoint item)))
    (assert (not (null lrp)))
    (pushnew (cons lrp (lrpoint-lahead lrpoint))
             (spontaneous-laheads item)
             :test #'equal)))

(defun generate-laheads (items eof)
  (dolist (item items)
    (dolist (lrpoint (item-kernel item))
      (when (and (eq +START+ (nterm-name (rule-left (lrpoint-rule lrpoint))))
                 (zerop (lrpoint-pos lrpoint)))
        (pushnew (cons lrpoint (list eof))
                 (spontaneous-laheads item)
                 :test #'equal))
      (let* ((test-lrpoint (make-lrpoint :rule   (lrpoint-rule lrpoint)
                                         :pos    (lrpoint-pos  lrpoint)
                                         :lahead (list +wildcard+)))
             ;; TODO modify closure-lr1 so that it could use other slots
             ;; for lookahead.  Or will it break lrpoint= ?
             (closure (closure-lr1 (list test-lrpoint))))
        (loop :for lrp :in closure :do
              (let ((nt (nterm-at-pos lrp)))
                (when nt
                  (let ((next (cdr (assoc nt (item-moves item)))))
                    (assert next)
                    (if (equal `(,+wildcard+) (lrpoint-lahead lrp))
                        (set-channel-info item lrpoint
                                          next (advance-lrpoint lrp))
                        (set-spontaneous-lahead-info
                         (advance-lrpoint lrp) next))))))))))


;; TODO: keep lookaheads as (lrpoint lahead1 lahead2 ...), not as
;; (lrpoint . lahead1), (lrpoint . lahead2) ...  But it will affect
;; calculation of new-unprocessed.
;;
;; Note that each lookahead is one-element list of terminals.
(defun spread-laheads (items)
  (let ((unprocessed items))
    (loop
     :for new-unprocessed := nil
     :while unprocessed :do
     ;; TODO: keep (CONS ITEM LRP) in new-unprocessed, not just ITEM 
     (loop
      :for item :in unprocessed
      :for sp-laheads := (spontaneous-laheads item)
      :when sp-laheads :do
      (loop
       :for (from-lrp to-item . to-lrp)
       :in (channels item)
       :for old-sp-laheads := (spontaneous-laheads to-item) :do
       (loop
        :for (lrp . lahead) :in sp-laheads
        :when (eq lrp from-lrp) :do
        (pushnew (cons to-lrp lahead)
                 (spontaneous-laheads to-item)
                 :test #'equal))
       (unless (eq old-sp-laheads (spontaneous-laheads to-item))
         (pushnew to-item new-unprocessed))))
     (dolist (item unprocessed)
       (let ((sp-laheads (spontaneous-laheads item)))
         (when sp-laheads)))
     (setf unprocessed new-unprocessed))))

(define-property rm-info)

(defun update-rm-info (target-nterminal rule)
  "Update RM-INFO for TARGET-NTERMINAL with information from RULE."
  (if (or (epsilon-rule-p rule) (terminal-p (first (rule-right rule))))
      nil
      (let ((source (first (rule-right rule)))
            (tail (rest (rule-right rule)))
            (updated-p nil))
        (let ((source-rm-info (rm-info source))
              (target-rm-info (rm-info target-nterminal))
              (first-set (seq-first tail)))
          (dotimes (i (array-dimension source-rm-info 0))
            ;; Try uniting info about i-th nterminal
            (when (aref source-rm-info i)
              (let ((new-value (ounion (aref target-rm-info i)
                                       (combine-first (aref source-rm-info i)
                                                      first-set)
                                       :ordering #'nterm<=)))
                (unless (equal new-value (aref target-rm-info i))
                  (setf (aref target-rm-info i) new-value
                        updated-p t))))))
        updated-p)))

(defun calculate-rm-info (grammar)
  "Info about rightmost derivations"
  (let ((nterminal-num (length (grammar-nterminals grammar)))
        (terminal-num (length (grammar-terminals grammar))))
    (dolist (nterminal (grammar-nterminals grammar))
      (let ((array (make-array nterminal-num :initial-element nil)))
        (setf (aref array (- (nterm-id nterminal) terminal-num))
              '(nil)) ; Or nil?
        (setf (rm-info nterminal) array)))
    (let ((set (grammar-nterminals grammar)))
      (loop :while set
            :for updated-nterminals := nil :do
            (dolist (nterminal set)
              (let ((updated nil))
                (dolist (rule (nterm-rules nterminal))
                  (when (update-rm-info nterminal rule)
                    (setf updated t)))
                (when updated
                  (pushnew nterminal updated-nterminals))))
            (setf set updated-nterminals)))))

;;; TODO: try specialized version.  Necessary info can be generated
;;; during calculation of FIRST.
(defun items-lalr (grammar)
  (calculate-rm-info grammar)
  (let ((lr0-items (items-lr0 grammar)))
    (dolist (item lr0-items)
      (setf (item-kernel item)
            (kernel (item-set item))))
    (generate-laheads lr0-items (nterm-by-name +EOF+ grammar))
    (spread-laheads lr0-items)
    lr0-items))

(defun nterminal-epsilon-rules (nterminal)
  "List of epsilon rules of the nterminal"
  (mapcan #'(lambda (rule)
              (if (epsilon-rule-p rule)
                  (list rule)
                  nil))
          (nterm-rules nterminal)))

(defun get-epsilon-reductions (lrpoint lahead grammar)
  "Epsilon reductions that can happen for the LRPOINT and LAHEAD"
  (let ((nterm (nterm-at-pos lrpoint)))
    (if (or (not nterm) (terminal-p nterm))
        nil
        (let ((accumulator ())
              (rm-info (rm-info nterm)))
          (loop :for i :from 0 :below (array-dimension rm-info 0)
                :for nterminal :in (grammar-nterminals grammar) :do
                (let ((first-set (combine-first-sets
                                  (aref rm-info i)
                                  (seq-first
                                   (nthcdr (1+ (lrpoint-pos lrpoint))
                                           (rule-right (lrpoint-rule lrpoint))))
                                  lahead)))
                  (loop :for rule :in (nterminal-epsilon-rules nterm) :do
                        (loop :for terminal :in first-set :do
                              (push (cons rule (list terminal))
                                    accumulator)))))
          (nreverse accumulator)))))


(defun reduce-set-lalr (item grammar)
  (mapcan #'(lambda (pair)
              (destructuring-bind (lrpoint . lahead) pair
                  (if (reduction-lrpoint-p lrpoint)
                      (list (cons (lrpoint-rule lrpoint)
                                  lahead))
                      (get-epsilon-reductions lrpoint lahead grammar))))
          (spontaneous-laheads item)))
