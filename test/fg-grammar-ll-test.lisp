;;; ** General LL expansion
(rt:deftest (fucc-generator::expand-rules-ll :simple)
    ;; There is nothing to expand
    (fucc-generator::expand-rules-ll '((s nil #'list a b c)))
  ;; Result
  ((s nil #'list a b c)))

;;; ** Expanding forms
(rt:deftest (fucc-generator::expand-form-ll :or)
    (multiple-value-bind (nterm rules trans)
        (fucc-generator::expand-form-ll
         '(:or a ((:list a b)) (b) (b c))
         nil
         0)
      (list
       (every (lambda (rule)
                (eq nterm (first rule)))
              rules)
       (mapcar #'rest rules)
       trans))
  (t ((nil a) (nil (:list a b)) (nil b) (nil b c)) nil))

(rt:deftest (fucc-generator::expand-form-ll :*)
    (multiple-value-bind (nterm rules trans)
        (fucc-generator::expand-form-ll
         '(:* a (:list a b) b c)
         nil
         0)
      (sublis (list (cons nterm 'no-such-symbol1)) rules))
  ((no-such-symbol1 nil (constantly nil))
   (no-such-symbol1 nil #'list* a (:list a b) b c no-such-symbol1)))

(rt:deftest (fucc-generator::expand-form-ll :+)
    (multiple-value-bind (nterm rules trans)
        (fucc-generator::expand-form-ll
         '(:+ a (:list a b) b c)
         nil
         0)
      (sublis (list (cons nterm 'no-such-symbol2)
                    (cons (caadr rules) 'no-such-symbol1)) rules))
  ((no-such-symbol2 nil #'list* a (:list a b) b c no-such-symbol1)
   (no-such-symbol1 nil (constantly nil))
   (no-such-symbol1 nil #'list* a (:list a b) b c no-such-symbol1)))

(rt:deftest (fucc-generator::expand-form-ll :maybe 1)
    (multiple-value-bind (nterm rules trans)
        (fucc-generator::expand-form-ll
         '(:maybe a)
         nil
         0)
      (sublis (list (cons nterm 'no-such-symbol1)) rules))
  ((no-such-symbol1 nil (constantly nil))
   (no-such-symbol1 nil #'identity a)))

(rt:deftest (fucc-generator::expand-form-ll :maybe 2)
    (multiple-value-bind (nterm rules trans)
        (fucc-generator::expand-form-ll
         '(:maybe a (:list a b) b c)
         nil
         0)
      (sublis (list (cons nterm 'no-such-symbol1)) rules))
  ((no-such-symbol1 nil (constantly nil))
   (no-such-symbol1 nil #'list a (:list a b) b c)))

(rt:deftest (fucc-generator::expand-form-ll :list)
    (multiple-value-bind (nterm rules trans)
        (fucc-generator::expand-form-ll
         '(:list a b)
         nil
         0)
      (sublis (list (cons nterm 'no-such-symbol2)
                    (cons (caadr rules) 'no-such-symbol1)) rules))
  ((no-such-symbol2 nil #'cons a no-such-symbol1)
   (no-such-symbol1 nil (constantly nil))
   (no-such-symbol1 nil  (lambda (second first rest)
                                              (cons first rest))
                    b a no-such-symbol1)))

;;;; Local variables: ***
;;;; mode: lisp ***
;;;; outline-regexp: ";;;; \\*\\*+ " ***
;;;; End: ***
