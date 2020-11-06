(defpackage :cp/test/radix-heap
  (:use :cl :fiveam :cp/radix-heap :cp/binary-heap)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/radix-heap)
(in-suite base-suite)

(define-binary-heap bheap
  :order #'<
  :element-type fixnum)

(test radix-heap/manual
  (let ((h (make-radix-heap)))
    (is (rheap-empty-p h))
    (is (= 0 (rheap-count h)))
    (rheap-push 15 h)
    (is (not (rheap-empty-p h)))
    (dolist (x '(6 18 220 270 9 11))
      (rheap-push x h))
    (is (= 6 (rheap-pop h)))
    (rheap-push 10 h)
    (is (= 7 (rheap-count h)))
    (is (equal '(9 10 11 15 18 220 270)
                   (loop repeat 7 collect (rheap-pop h))))
    (signals rheap-not-monotone-error (rheap-push 269 h))
    (signals rheap-empty-error (rheap-pop h))
    (rheap-push 270 h)
    (rheap-push 270 h)
    (is (= 270 (rheap-pop h)))
    (is (= 270 (rheap-pop h)))
    (is (rheap-empty-p h))
    (rheap-push #xffffffff h)
    (is (= #xffffffff (rheap-pop h)))
    (locally (declare #+sbcl (sb-ext:muffle-conditions warning))
      (signals type-error (rheap-push (ash 1 +radix-heap-bit-depth+) h)))))

(test radix-heap/random
  (let ((state (sb-ext:seed-random-state 0)))
    (finishes
      (dotimes (_ 100)
        (let ((rheap (make-radix-heap))
              (bheap (make-bheap 1000)))
          (dotimes (_ 1000)
            (if (or (zerop (rheap-count rheap)) (= (random 2 state) 1))
                (let* ((lowest (rheap-lowest rheap))
                       (rand (random most-positive-fixnum state))
                       (delta (logand (- (ash 1 (random 15 state)) 1) rand)))
                  (rheap-push (+ lowest delta) rheap)
                  (bheap-push (+ lowest delta) bheap))
                (assert (= (rheap-pop rheap) (bheap-pop bheap))))))))))
