(defpackage :cp/xor
  (:use :cl)
  (:export #:xor))
(in-package :cp/xor)

(defmacro xor (form1 form2)
  (let ((f1 (gensym)) (f2 (gensym)))
    `(let ((,f1 ,form1)
           (,f2 ,form2))
       (if ,f1 (not ,f2) ,f2))))
