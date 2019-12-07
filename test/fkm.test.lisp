(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../fkm.lisp")
  (load "./set-equal.lisp"))

(use-package :test-util)

(defun collect-necklaces (length alphabet-size)
  (let (res)
    (map-necklaces
     (lambda (x) (push (copy-seq x) res))
     length
     alphabet-size)
    res))

(with-test (:name fkm)
  (assert (set-equal (collect-necklaces 4 2)
                     '(#(0 0 0 0)
                       #(0 0 0 1)
                       #(0 0 1 1)
                       #(0 1 0 1)
                       #(0 1 1 1)
                       #(1 1 1 1))
                     :test #'equalp))
  (assert (set-equal (collect-necklaces 5 1)
                     '(#(0 0 0 0 0))
                     :test #'equalp))
  (assert (set-equal (collect-necklaces 1 5)
                     '(#(0) #(1) #(2) #(3) #(4))
                     :test #'equalp))
  (assert (set-equal (collect-necklaces 0 4)
                     '(#())
                     :test #'equalp)))
