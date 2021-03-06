(defpackage :cp/test/read-line-into
  (:use :cl :fiveam :cp/read-line-into :cp/test/octet-input-stream)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/read-line-into)
(in-suite base-suite)

(test read-line-into
  (let ((buf (make-string 5 :element-type 'base-char))
        (*standard-input* (make-octet-input-stream "foo")))
    (is (equalp "foo  " (read-line-into buf))))
  (let ((buf (make-string 3))
        (*standard-input* (make-octet-input-stream "foo")))
    (is (equalp "foo" (read-line-into buf)))))

