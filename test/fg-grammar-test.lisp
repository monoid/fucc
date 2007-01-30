;;;; ** Grammar parsing
(rt:deftest (fucc-generator::split-rule-form :simple)
    (fucc-generator::split-rule-form '(s -> a b c (:do (print 'abc))))
  ;; Result
  ((s a b c (:do (print 'abc)))))

(rt:deftest (fucc-generator::split-rule-form :multiple)
    (fucc-generator::split-rule-form '(s -> a b c (:do (print 'abc))
                                       -> (:do (print 'car)) c a r))
  ;; Result
  ((s a b c (:do (print 'abc)))
   (s (:do (print 'car)) c a r)))

(rt:deftest (fucc-generator::split-rule-form :separator)
    (fucc-generator::split-rule-form '(s :=> a b c (:do (print 'abc))
                                       :=> (:do (print 'car)) c a r))
  ;; Result
  ((s a b c (:do (print 'abc)))
   (s (:do (print 'car)) c a r)))

(rt:deftest (fucc-generator::expand-action :simple)
    (fucc-generator::expand-action '(s  a b c (:call (function +))))
  ;; Result
  (s () (function +) a b c))

(rt:deftest (fucc-generator::expand-action :insert-action)
    (fucc-generator::expand-action '(s  a b c))
  ;; Result
  (s () (function list) a b c))

(defun replace-arguments (form arguments-extractor replace)
  (let ((args (funcall arguments-extractor form)))
    (sublis
     (loop
        :for arg :in args
        :for new-arg :in replace
        :collect (cons arg new-arg))
     form)))

(rt:deftest (fucc-generator::expand-action :class)
    (replace-arguments
     (fucc-generator::expand-action '(s
                                      (:initarg :a a)
                                      b
                                      (:initarg :c c)
                                      (:class test)))
     (lambda (form) (cadar (cdaddr form)))
     '(a0 b1 c2))
  ;; Result
  (s () (function (lambda (a0 b1 c2)
          (declare (ignorable c2 b1 a0))
          (make-instance 'test :a a0 :c c2)))
     a b c))

(rt:deftest (fucc-generator::expand-action :form)
    (fucc-generator::expand-action '(s
                                     (:do (print "Hello, world!"))
                                     (:var a1 a)
                                     (:var b (:or b c))
                                     (:var a2 a)
                                     (:do (list a1 a2 b))))
  ;; Result
  (s ()
     (function
      (lambda (a1 b a2)
       (declare (ignorable a2 b a1))
       (list a1 a2 b)))
     (:call (function
             (lambda ()
              (declare (ignorable))
              (print "Hello, world!"))))
     a (:or b c) a))

(rt:deftest (fucc-generator::apply-argument-transforms-to-action :simple)
    (replace-arguments
     (fucc-generator::apply-argument-transforms-to-action
      '(nil nil reverse nil)
      '(lambda (a b c d) (list a d b c)))
     #'cadadr
     '(k l m n))
  ;; Result
  (function
    (lambda (k l m n)
     (funcall (lambda (a b c d) (list a d b c)) k l (reverse m) n))))

;;;; Local variables: ***
;;;; mode: lisp ***
;;;; outline-regexp: ";;;; \\*\\*+ " ***
;;;; End: ***
