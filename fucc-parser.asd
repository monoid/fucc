#| -*- mode: lisp; -*-
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

(asdf:defsystem #:fucc-parser
    :name "LR-parser"
    :author "Ivan Boldyrev"
    :version "0.2.2"
    :license "MIT"
    :description "LL and LR/LALR parser runtime system"
    :components ((:module "parser"
                          :components
                          ((:file "fucc-package")
                           (:file "fucc-decl"
                                  :depends-on ("fucc-package"))
                           (:file "fucc-util"
                                  :depends-on ("fucc-package" "fucc-decl"))
                           (:file "fucc-parser"
                                  :depends-on ("fucc-package" "fucc-decl"
                                                              "fucc-util"))
                           (:file "fucc-ll"
                                  :depends-on ("fucc-package"))))))

; (asdf:operate 'asdf:load-op :fucc-parser)
