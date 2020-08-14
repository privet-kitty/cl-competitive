(defpackage :cp/test/relative-error
  (:use :cl :fiveam :cp/relative-error)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/relative-error)
(in-suite base-suite)

(test relative-error
  (is (relative-error<= 10000000000d0 10000000001d0 1d-9))
  (is (relative-error<= 10000000001d0 10000000000d0 1d-9))
  (is (relative-error<= 10000000000d0 10000000001d0 1f-9))
  (is (relative-error<= 10000000001d0 10000000000d0 1f-9))
  (is (not (relative-error<= 1d-30 1d-25 1d-9)))
  (is (not (relative-error<= 1d-25 1d-30 1d-9)))
  (is (not (relative-error<= 0 1d-30 1d-9)))
  (is (not (relative-error<= 1d-30 0 1d-9)))
  (is (error<= 1d-30 1d-25 1d-9))
  (is (error<= 1d-25 1d-30 1d-9)))
