(defpackage :cp/test/main
  (:use #:cl #:fiveam)
  (:export #:main-suite))

(in-package :cp/test/main)

(def-suite main-suite)
(in-suite main-suite)
