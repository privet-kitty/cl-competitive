(defpackage :cp/prefix-p
  (:use :cl)
  (:export #:prefix-p #:suffix-p))
(in-package :cp/prefix-p)

(declaim (inline prefix-p))
(defun prefix-p (prefix sequence &key (test #'eql))
  "Returns true iff PREFIX is a prefix of SEQUENCE."
  (let ((pos (mismatch prefix sequence :test test)))
    (or (null pos)
        (= pos (length prefix)))))

(declaim (inline suffix-p))
(defun suffix-p (suffix sequence &key (test #'eql))
  "Returns true iff SUFFIX is a suffix of SEQUENCE."
  (let ((pos (mismatch suffix sequence :test test :from-end t)))
    (or (null pos)
        (zerop pos))))
