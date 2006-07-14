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

(cl:in-package #:fucc)

;;; Implement goto-table as binary-search array

(defun bin-search-goto-table (table key)
"Binary search in array of cons pairs: (key . value).
Because of nature of goto-table search is always successfull, so error check
are not performed.
Returns value associated with the KEY."
  (declare (type (simple-array (cons term state) (*)) table)
           (type term key))
  (let ((lower 0)
        (upper (array-dimension table 0)))
    (loop :for a :of-type fixnum := lower :then (if lower-p    a  mid)
          :and b :of-type fixnum := upper :then (if lower-p  mid    b)
          :for mid :of-type fixnum := (ash (+ a b) -1)
          :for midkey :of-type term := (car (aref table mid))
          :for lower-p := (< key midkey)
          :when (= key midkey) :do
            (return (cdr (aref table mid))))))

(defun alist-to-hash-table (list)
  (let ((ht (make-hash-table)))
    (loop :for (key . val) :in list :do
          (setf (gethash key ht) val))
    ht))
