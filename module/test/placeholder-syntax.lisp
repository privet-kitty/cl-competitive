(defpackage :cp/test/placeholder-syntax
  (:use :cl :fiveam :cl-syntax :cp/placeholder-syntax)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/placeholder-syntax)
(in-suite base-suite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-syntax:defsyntax placeholder-syntax
    (:merge :standard)
    (:dispatch-macro-char #\# #\% #'read-placeholder-form)))

(cl-syntax:use-syntax placeholder-syntax)

(test placeholder-syntax
  (is (equal "adsb" (#%(uiop:strcat "a" %3 "s" %1) "b" "c" "d")))
  (is (equal "bab" (#%(uiop:strcat % "a" %) "b"))))
