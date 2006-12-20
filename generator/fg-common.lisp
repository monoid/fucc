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

(let ((processed-lrpoints (make-hash-table :test 'eql))
      (processed-nterminals (make-hash-table :test 'equal))
      (new-unprocessed (make-hash-table :test 'equal)))
  (defun closure (set proceed expand ordering)
    (declare (type function proceed expand ordering))
    (let ((unprocessed 'nil))
      ;; Initial set of non-kernel nterminals
      (dolist (elt set)
        (dolist (new-elt (funcall proceed elt))
          (unless (gethash new-elt processed-nterminals)
            (push new-elt unprocessed))))
      (loop :while unprocessed :do
            (clrhash new-unprocessed)
            ;; Mark as processed _before_ processing
            (dolist (unpr unprocessed)
              (setf (gethash unpr processed-nterminals) t))
            ;; Process
            (dolist (unpr unprocessed)
              (dolist (pt (funcall expand unpr))
                ;; Proceed: new possibilities
                (when (eq pt (iget pt processed-lrpoints
                                   #'lrpoint-hash
                                   #'lrpoint=))
                  (dolist (candidate (funcall proceed pt))
                    (when (and candidate
                               (not (gethash candidate processed-nterminals)))
                      (iget candidate new-unprocessed #'sxhash #'equal))))))
            (setf unprocessed (itable-to-list new-unprocessed)))
      (prog1
          (sort (nconc
                 (itable-to-list processed-lrpoints)
                 set)
                ordering)
        (clrhash processed-lrpoints)
        (clrhash processed-nterminals)
        (clrhash new-unprocessed)))))

(let ((data (make-hash-table :test 'eql)))
  (defun goto-nc (set nterminal pred)
    (loop :for lrp :in set
          :for rule := (lrpoint-rule lrp)
          :for pos := (lrpoint-pos lrp)
          :when (and (< pos (rule-length rule))
                     (funcall pred nterminal rule pos))
          :do  (iget (advance-lrpoint lrp) data #'lrpoint-hash #'lrpoint=))
    (prog1
        (sort (itable-to-list data) #'lrpoint<=)
      (clrhash data))))

(defun items (grammar closure-fun goto-fun initial-lrpoint)
  "Calculate kernel items.  Returns list of items."
  (let* ((root-item (make-item
                     :set
                     (funcall closure-fun (list initial-lrpoint))))
         (items (make-hash-table :test 'eql))
         (unprocessed (list root-item)))
    (loop :while unprocessed :do
          (let ((new-unprocessed nil))
            (dolist (item unprocessed)
              (iget item items #'item-hash #'item=))
            (dolist (nterminal (append (grammar-terminals grammar)
                                   (grammar-nterminals grammar)))
              (dolist (item unprocessed)
                (let ((new-set (funcall goto-fun (item-set item) nterminal)))
                  (when new-set
                    (let* ((new-item (make-item :set new-set))
                           (member (iget new-item items
                                         #'item-hash
                                         #'item=)))
                      (when (eq new-item member)
                        ;; The item is new
                        (push member new-unprocessed))
                      (push (cons nterminal member)
                            (item-moves item)))))))
            (setf unprocessed new-unprocessed)))
    (cons root-item
          (delete root-item 
                  (loop :for list :being :each :hash-value :of items
                        :nconc list)))))

(defun reduction-lrpoint-p (lrp)
  "Check if lrpoint is reduction lrpoint (i.e. point is at right side)"
  (or (= (lrpoint-pos lrp)
         (rule-length (lrpoint-rule lrp)))
      ;; Special care for epsilon-rules
      (and (epsilon-rule-p (lrpoint-rule lrp))
           (zerop (lrpoint-pos lrp)))))

(defun report-conflict-and-ret (type used-action discarded-action)
  (warn "~S conflict: ~S is used over ~S." type used-action discarded-action)
  used-action)

(defun compare-actions (action1 action2)
  (let ((priority1 (prec-priority (cadr action1)))
           (priority2 (prec-priority (cadr action2))))
    (assert (and priority1 (<= 0 priority1)))
    (assert (and priority2 (<= 0 priority2)))
    (cond
      ((< priority1 priority2) :less)
      ((> priority1 priority2) :more)
      (t ; equal
       (cond
         (; Reduce/reduce
          (and (eq :reduce (car action1))
               (eq :reduce (car action2)))
          :uncomparable)
         (; Shift/reduce
          (and (eq :shift (car action1))
               (eq :reduce (car action2)))
          (ecase (prec-assoc (cadr action1)) ; Same priority -> same
					     ; associativity
            (:left
             :less)
            (:right
             :more)
            ((:none :nonassoc)
             :nonassoc)))
         (; Reduce/shist
          (and (eq :reduce (car action1))
               (eq :shift (car action2)))
          (ecase (prec-assoc (cadr action1)) ; Same priority -> same
					     ; associativity
            (:left
             :more)
            (:right
             :left)
            ((:none :nonassoc)
             :nonassoc)))
         (; priority error
          t #|(or (eq :priority-error (car action1))
                  (eq :priority-error (car action2)))|#
          :nonassoc))))))

(defun add-to-maximal-actions (max-actions action)
  (cons action     ; Warning: non-local exit with RETURN-FROM may skip
                                        ; this CONS.
        (loop :for actions :on max-actions
              :for cmp := (compare-actions action (first actions))
              :if (eq :less cmp) :do
              ;; New action is less than some old one.  Drop new action
              (return-from add-to-maximal-actions
                (nconc actions processed))
              :else :if (eq :uncomparable cmp)
              :collect (first actions) :into processed
              :else :if (eq :nonassoc cmp)
              :collect (list :priority-error (prec-priority (second action)))
                 :into processed
              :end
              :finally (return processed))))

(defun resolve-simple-conflicts (actions)
  "Return set of actions that no other action supersedes."
  (loop :with no-priority := nil
        :with has-priority := nil
        :for action :in actions
        :for priority := (prec-priority (second action))
        :if (or (null priority)
                (minusp priority))
        :do (push action no-priority)
        :else
        :do (setf has-priority
                  (add-to-maximal-actions has-priority action))
        :end
        :finally (return (nconc
                          ;; Deleter priority error states
                          (delete :priority-error has-priority
                                  :key #'car)
                          no-priority))))

(defun linearize-conflicts (actions)
  (reduce #'(lambda (a1 a2)
              (cond
                ((eq :shift (first a1))
                 (report-conflict-and-ret :shift/reduce a1 a2))
                ((eq :shift (first a2))
                 (report-conflict-and-ret :shift/reduce a2 a1))
                (t ; Reduce/reduce
                 (if (< (rule-index (cadr a1))
                        (rule-index (cadr a2)))
                     (report-conflict-and-ret :reduce/reduce a1 a2)
                     (report-conflict-and-ret :reduce/reduce a2 a1)))))
          (resolve-simple-conflicts actions)))

(defun generate-tables (grammar items reduce-set-fun)
  "Return two values: action table and goto table."
  (loop :for item :in items
        :for num :from 0 :do
        (setf (item-index item) num))
  (let ((state-num (length items))
        (terminals-num (length (grammar-terminals grammar)))
        (nterminals-num (length (grammar-nterminals grammar))))
    (let ((action-table (make-array (list state-num terminals-num)
                                    :initial-element nil))
          (goto-table (make-array (1- nterminals-num) :initial-element nil)))
      ;; Shifts
      (dolist (item items)
        (loop :for (nterm . new-item) :in (item-moves item) :do
              (if (terminal-p nterm)
                  (pushnew (list* :shift nterm (item-index new-item))
                           (aref action-table
                                 (item-index item)
                                 (nterm-id nterm))
                           :test #'equal)
                  (pushnew (cons (item-index item)
                                 (item-index new-item))
                           (aref goto-table
                                 (- (nterm-id nterm)
                                    terminals-num))
                           :test #'equal))))
      ;; Reduce or accept
      (dolist (item items)
        (dolist (redex (funcall reduce-set-fun item grammar))
          (destructuring-bind (rule nterm) redex
            (pushnew (if (eql +START+
                              (nterm-name (rule-left rule)))
                         (list :accept)
                         (cons :reduce redex))
                     (aref action-table
                           (item-index item)
                           (nterm-id nterm))
                     :test #'equal))))
      (values action-table goto-table))))

(defun chain-rule-p (rule)
  (let ((left-side (rule-left rule)))
    (and (consp left-side)
         (not (null (first left-side)))
         (null (rest left-side)))))

(defun one-step-chain-rule-p (rule state terminal-id action-table goto-table)
  (assert (chain-rule-p rule))
  (let* ((idx (nterm-id (first (rule-left rule))))
         (new-state (aref goto-table idx state))
         (new-actions (aref action-table new-state terminal-id)))
    (find :action new-actions :key #'car)))

(defun reduce-action (action state terminal action-table goto-table)
  (declare (ignore action state terminal action-table goto-table))
  (error "REDUCE-ACTION is unimplemented yet"))

(defun remove-chain-rules (action-table goto-table)
  (let ((unprocessed nil))
    ;; Initialize list of unprocessed items
    (dotimes (i (array-dimension action-table 0))
      (dotimes (j (array-dimension action-table 1))
        (loop :for action-tail :on (aref action-table i j)
              :for action := (first action-tail) :do
              (when (and action-tail
                         (eq :reduce (first action))
                         (chain-rule-p (cadr action)))
                (push (list i j action action-tail) unprocessed)))))
    ;; do processing
    (loop :with repeat-flag := t :do
          (let ((new-unprocessed nil))
            (setf repeat-flag nil)
            (loop :for unproc :in unprocessed :do
                  (destructuring-bind (state terminal action action-tail) unproc
                    (if (one-step-chain-rule-p (cadr action)
                                               state
                                               terminal
                                               action-table
                                               goto-table)
                        (setf (first action-tail)
                              (reduce-action action
                                             state
                                             terminal
                                             action-table
                                             goto-table)
                              ;; Set repeat-flag
                              repeat-flag
                              t)
                        (push unproc new-unprocessed))))
            (setf unprocessed new-unprocessed))
          :while repeat-flag)))
