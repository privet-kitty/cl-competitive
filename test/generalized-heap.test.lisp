(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util.lisp")
  (load "../generalized-heap.lisp"))

(use-package :test-util)

(define-binary-heap test-heap
  :order #'<
  :element-type (unsigned-byte 32))

(with-test (:name generalized-heap)
  (let ((h (make-test-heap 7)))
    (dolist (o (list 7 18 22 15 27 9 11))
      (test-heap-push o h))
    (signals heap-full-error (test-heap-push 0 h))
    (assert (= 7 (test-heap-peak h)))
    (assert (equal '(7 9 11 15 18 22 27)
                   (loop repeat 7 collect (test-heap-pop h))))
    (assert (test-heap-empty-p h))
    (signals heap-empty-error (test-heap-pop h))
    (assert (eql 'eof (test-heap-pop h nil 'eof)))
    (assert (eql 'eof (test-heap-peak h nil 'eof))))
  (assert (typep (test-heap-data (make-test-heap 10))
                 '(simple-array (unsigned-byte 32) (*)))))
