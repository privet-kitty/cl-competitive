(defpackage :cp/test/buffered-read-line
  (:use :cl :fiveam :cp/buffered-read-line :cp/test/octet-input-stream)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/buffered-read-line)
(in-suite base-suite)

(test buffered-read-line
  (let ((*standard-input* (make-octet-input-stream "foo")))
    (is (equalp "foo  " (buffered-read-line 5))))
  (let ((*standard-input* (make-octet-input-stream "foo")))
    (is (equalp "foo" (buffered-read-line 3)))))
