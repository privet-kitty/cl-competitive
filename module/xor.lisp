(defpackage :cp/xor
  (:use :cl)
  (:export #:xor))
(in-package :cp/xor)

(defmacro xor (&rest forms)
  "When there is only one truthy form in FORMS, its value is returned. Otherwise
returns NIL. Note that this macro doens't do short-circuit evaluation, i.e.,
every form is evaluated in order."
  (let ((syms (loop for _ in forms collect (gensym "CLAUSE")))
        (result (gensym "RESULT"))
        (name (gensym "XOR")))
    `(block ,name
       (let (,@(loop for sym in syms
                     for form in forms
                     collect (list sym form))
             ,result)
         ,@(loop for sym in syms
                 for form in forms
                 collect `(if ,result
                              (when ,sym
                                (return-from ,name))
                              (setq ,result ,sym)))
         ,result))))
