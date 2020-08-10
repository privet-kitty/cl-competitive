(defpackage :cp/test/abstract-heap
  (:use :cl :fiveam :cp/abstract-heap)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/abstract-heap)
(in-suite base-suite)

(define-binary-heap test-heap
  :order #'<
  :element-type (unsigned-byte 32))

(test abstract-heap/manual
  (let ((h (make-test-heap 7)))
    (is (= 0 (test-heap-count h)))
    (dolist (x '(6 18 22 15 27 9 11))
      (test-heap-push x h))
    (is (= 6 (test-heap-peek h)))
    (is (= 7 (test-heap-count h)))
    (test-heap-push 0 h)
    (is (= 8 (test-heap-count h)))
    (is (= 0 (test-heap-peek h)))
    (test-heap-push 7 h)
    (is (= 9 (test-heap-count h)))
    (is (= 0 (test-heap-peek h)))
    (is (zerop (test-heap-pop h)))
    (is (= 8 (test-heap-count h)))
    (is (= 6 (test-heap-peek h)))
    (is (equal '(6 7 9 11 15 18 22 27)
                   (loop repeat 8 collect (test-heap-pop h))))
    (is (test-heap-empty-p h))
    (signals heap-empty-error (test-heap-pop h)))
  (is (typep (test-heap-data (make-test-heap 10))
             '(simple-array (unsigned-byte 32) (*)))))
