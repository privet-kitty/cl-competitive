(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../heap.lisp"))

(use-package :test-util)


(with-test (:name heap)
  (let ((h (make-heap 7 :test #'< :element-type '(unsigned-byte 32))))
    (dolist (o (list 7 18 22 15 27 9 11))
      (heap-push o h))
    (signals heap-full-error (heap-push 0 h))
    (assert (= 7 (heap-peek h)))
    (assert (equal '(7 9 11 15 18 22 27)
                   (loop repeat 7 collect (heap-pop h))))
    (assert (heap-empty-p h))
    (signals heap-empty-error (heap-pop h)))
  (assert (typep (heap-data (make-heap 10 :element-type 'list))
                 '(simple-array list (*)))))
