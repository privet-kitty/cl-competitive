(defpackage :cp/test/seek-line
  (:use :cl :fiveam :cp/seek-line)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/seek-line)
(in-suite base-suite)

(test seek-line
  (let ((in (make-string-input-stream (format nil "~Casd~C~C~Cuiop~C~C~Ca~C"
                                              #\Newline
                                              #\Newline #\Newline #\Return
                                              #\Return #\Return #\Newline
                                              #\Return))))
    (seek-line in)
    (is (equal "asd" (read-line in)))
    (is (equal "" (read-line in)))
    (seek-line in)
    (is (equal "uiop" (string-right-trim '(#\Return) (read-line in))))
    (seek-line in)
    (is (equal "a" (string-right-trim '(#\Return) (read-line in))))
    (is (null (read-line in nil nil)))))
