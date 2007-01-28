#+asdf(eval-when (:compile-toplevel :execute :load-toplevel)
        (asdf:oos 'asdf:load-op :fucc-parser))
#+asdf(eval-when (:compile-toplevel :execute)
        (asdf:oos 'asdf:load-op :fucc-generator))

;;;  Calculators are "hello world" for parsers

(defparameter *dictionary*
  (make-hash-table)
  "Table that associate variable with value")

;;;  Adding parenthes ( exp ) is left as exercise
(fucc:defparser *calculator-parser*
    s ; Initial non-terminal
  ;; List of terminal
  (+ - * / = :semicolon :id :const)
  ;; List of rules
  ((s   (:form (format t "Value: ~S" (first (last exp-list))))
      (:var exp-list (:list exp :semicolon))
      (:maybe :semicolon))
   ;; Assignment
   (exp (:form (setf (gethash var *dictionary*)
                       exp))
      (:var var :id) = (:var exp exp))
   ;; Binary operations
   (exp (:action (lambda (a op b)
                     (funcall op a b)))
      exp
      (:or ((:or + -)) ; Nested OR here is just for fun
           * /)
      exp)
   ;; Constants and variables
   (exp (:action #'identity)
      :const)
   (exp (:action (lambda (var)
                   (or (gethash var *dictionary*)
                       (error "Undefined variable: ~S" var))))
      :id))
  :prec-info
  ((:right =) ;; Actually associativity doesn't matter here because
	      ;; it is enforsed by rule structure anyway.
   (:left + -) ;; But here it matters.
   (:left * /)))

(defun calc-lexer (list)
  "Return lexical analizer for list of tokens"
  (lambda ()
    (let ((next-value (pop list)))
      (cond
        ((null next-value)
         (values nil nil))
        ((member next-value '(:semicolon #\;))
         (values :semicolon :semicolon))
        ((member next-value '(+ - * / =))
         (values next-value (fdefinition next-value)))
        ((symbolp next-value)
         (values :id next-value))
        ((numberp next-value)
         (values :const next-value))
        (t
         (error "Unknown token: ~S" next-value))))))

(defun test-calc (list)
  (fucc:parser-lr
   (calc-lexer list)
   *calculator-parser*))


(test-calc (copy-list '(a = c = 3 #\;
                        b = 4 #\;
                        a * a + b * 9 - a)))
