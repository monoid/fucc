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

(asdf:defsystem #:fucc-generator
    :name "LR-generator"
    :author "Ivan Boldyrev"
    :version "0.2.1"
    :depends-on ("fucc-parser")
    :components ((:module "generator"
                  :components
                  ((:file "fg-package")
                   (:file "fg-util"
                          :depends-on ("fg-package"))
                   (:file "fg-decl"
                          :depends-on ("fg-package"
                                       "fg-util"))
                   (:file "fg-grammar-lr"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-util"))
                   (:file "fg-grammar-ll"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-util"
                                       "fg-grammar-lr"))
                   (:file "fg-grammar"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-util"
                                       "fg-grammar-lr"
                                       "fg-grammar-ll"))
                   (:file "fg-transform"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-grammar"))
                   (:file "fg-common"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-util"
                                       "fg-grammar"))
                   (:file "fg-lr0"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-grammar"
                                       "fg-common"))
                   (:file "fg-lr"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-grammar"
                                       "fg-common"))
                   (:file "fg-lalr"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-grammar"
                                       "fg-common"
                                       "fg-lr0"
                                       "fg-lr"))
                   (:file "fg-ll"
                          :depends-on ("fg-package"
                                       "fg-grammar"
                                       "fg-common"))
                   (:file "fg-dump"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-grammar"
                                       "fg-common"))
                   (:file "fg-macro"
                          :depends-on ("fg-package"
                                       "fg-decl"
                                       "fg-grammar"
                                       "fg-grammar-lr"
                                       "fg-common"
                                       "fg-transform"
                                       "fg-lr0"
                                       "fg-lr"
                                       "fg-lalr"
                                       "fg-dump"))))))

; (asdf:operate 'asdf:load-op :fucc-generator)
