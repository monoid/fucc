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

(rt:deftest (fucc-generator::split-rule-form :warn)
    (block nil
      (handler-bind ((warning #'(lambda (&rest ingore)
                                  (declare (ignore ingore))
                                  (return :warn))))
        (fucc-generator::split-rule-form '(s a b c))))
  :warn)

(rt:deftest (fucc-generator::split-rule-form :nowarn)
    (block nil
      (handler-bind ((warning #'(lambda (&rest ingore)
                                  (declare (ignore ingore))
                                  (return :warn))))
        (fucc-generator::split-rule-form '(s -> a b c))))
  ((s a b c)))

(rt:deftest (fucc-generator::expand-action :simple)
    (fucc-generator::expand-action '(s  a b c (:call (function +))))
  ;; Result
  (s () (function +) a b c))

(rt:deftest (fucc-generator::expand-action :insert-action-0)
    (fucc-generator::expand-action '(s))
  ;; Result
  (s () (constantly nil)))

(rt:deftest (fucc-generator::expand-action :insert-action-1)
    (fucc-generator::expand-action '(s a))
  ;; Result
  (s () (function identity) a))

(rt:deftest (fucc-generator::expand-action :insert-action-1-with-do)
    (fucc-generator::expand-action '(s (:do nil) a))
  ;; Result
  (s () (function identity)
     (:call (function (lambda () (declare (ignorable)) nil)))
     a))

(rt:deftest (fucc-generator::expand-action :insert-action-1-with-call)
    (fucc-generator::expand-action '(s (:call (constantly 0)) a))
  ;; Result
  (s () (function identity)
     (:call (constantly 0))
     a))

(rt:deftest (fucc-generator::expand-action :insert-action-2)
    (fucc-generator::expand-action '(s a b))
  ;; Result
  (s () (function list) a b))

(rt:deftest (fucc-generator::expand-action :insert-action-3)
    (fucc-generator::expand-action '(s a b c))
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
