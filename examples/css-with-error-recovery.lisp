#+asdf(eval-when (:compile-toplevel :execute :load-toplevel)
        (asdf:oos 'asdf:load-op :fucc-parser))
#+asdf(eval-when (:compile-toplevel :execute)
        (asdf:oos 'asdf:load-op :fucc-generator))

(defun 1st (val &rest ignore)
  (declare (ignore ignore))
  val)

(defun 2nd (ignore1 val &rest ignore2)
  (declare (ignore ignore1 ignore2))
  val)

;; Simplified CSS parser with error recovery
(fucc:defparser *css-parser*
    s ; Initial non-terminal
  (:identifier :length :color
   #\; #\{ #\} #\( #\) #\: #\[ #\] #\*)
  ((s -> (* rule))
   (rule -> pattern body)
   (pattern -> :identifier) ; Real patterns may be complex
   (body -> #\{ decls #\} (:call #'2nd)
         -> #\{ decls error #\} (:call #'2nd))
   (decls -> 
          -> decl (:call (lambda (decl) (list decl)))
          -> decls #\; decl (:call (lambda (decls sep decl)
                                     (declare (ignore sep))
                                     ;; I know, it is slow, but short;
                                     ;; enough for example code, but
                                     ;; don't use it in real code
                                     (append decls (list decl))))
          -> decls #\;
          -> decls error #\; (:call (lambda (decls &rest ignore)
                                  (declare (ignore ignore))
                                  decls)))
   (decl -> :identifier #\: (:* property)
         -> :identifier #\: error #\;)
   ;; There are must also url, string etc., but this is just example.
   (property -> (:or :color :length :identifier))))

(defun dumb-lexer (list)
  (lambda ()
    (apply #'values (pop list))))

(defun test-css (list)
  (fucc:parser-lr
   (dumb-lexer list)
   *css-parser*))

#| CSS in this example:

* { color: #FFF; background: #000 }
p { border: 1px solid red; padding-left: 1em; }

|#
#-(and)
(test-css (copy-list '((:identifier "*")
                       (#\{         #\{)
                       (:identifier "color")
                       (#\:         #\:)
                       (:color      "#FFF")
                       (#\;         #\;)
                       (:identifier "background")
                       (#\:         #\:)
                       (:color      "#000")
                       (#\}         #\})
                       (:identifier "p")
                       (#\{         #\{)
                       (:identifier "border")
                       (#\:         #\:)
                       (:length     "1px")
                       (:identifier "solid")
                       (:identifier "red")
                       (#\;         #\;)
                       (:identifier "padding-left")
                       (#\:         #\:)
                       (:length     "1em")
                       (#\;         #\;)
                       (#\}         #\})
                       (nil         nil))))

(defun error-test ()
  (test-css (copy-list '((:identifier "*")
                         (#\{         #\{)
                         (:identifier "color")

                         (#\:         #\:)
                         (:color      "#FFF")
                         (#\;         #\;)
                         (:identifier "background")
                         (#\:         #\:)
                         (:color      "#000")
                         (#\}         #\})
                         (:identifier "p")
                         (#\{         #\{)
                         (:identifier "border")
                         (#\:         #\:)
                         (:length     "1px")
                         (:identifier "padding-left")
                         (#\:         #\:)
                         (:length     "1em")
                         (#\;         #\;)
                         (#\}         #\})
                         (nil         nil)))))
