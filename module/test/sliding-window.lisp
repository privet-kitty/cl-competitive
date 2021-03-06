(defpackage :cp/test/sliding-window
  (:use :cl :fiveam :cp/sliding-window)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/sliding-window)
(in-suite base-suite)

(test sliding-window-vector
  (declare (notinline swf-fold))
  (let* ((vector #(-5 4 9 3 11 -1 2 7 4 40 1 0))
         (swf (make-swf (lambda (i) (aref vector i)) #'> 12)))
    (is (= 4 (swf-fold swf 0 2)))
    (is (= 4 (swf-fold swf 1 2)))
    (is (= 9 (swf-fold swf nil 4)))
    (is (= 11 (swf-fold swf 3 5)))
    (is (= 11 (swf-fold swf 4 nil)))
    (is (= 7 (swf-fold swf 6 8)))
    (is (= 7 (swf-fold swf)))
    (is (= 4 (swf-fold swf 8 9)))
    (swf-extend swf)
    (is (= 40 (swf-fold swf)))
    (swf-shrink swf)
    (swf-shrink swf)
    (swf-extend swf)
    (is (= 1 (swf-fold swf)))
    (swf-extend swf)
    (is (= 1 (swf-fold swf)))
    (swf-shrink swf)
    (is (= 0 (swf-fold swf)))))
