(defpackage :cp/test/interactive
  (:use :cl :fiveam :cp/interactive)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/interactive)
(in-suite base-suite)

(defun solver ()
  (let ((n (read)))
    (dotimes (_ n)
      (format t "~D~%" (+ (read) (read)))
      (let ((ans (read-line)))
        (assert (equal "CORRECT" ans))))))

(defun grader ()
  (let ((n 10)
        (state (sb-ext:seed-random-state 0)))
    (format t "~D~%" n)
    (dotimes (_ 10)
      (let ((a (random most-positive-fixnum state))
            (b (random most-positive-fixnum state)))
        (format t "~D ~D~%" a b)
        (let ((sum (read)))
          (write-line (if (= sum (+ a b))
                          "CORRECT"
                          "WRONG")))))))

(test interact
  (finishes (interact #'solver #'grader)))
