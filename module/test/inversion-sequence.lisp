(defpackage :cp/test/inversion-sequence
  (:use :cl :fiveam :cp/inversion-sequence :cp/shuffle)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/inversion-sequence)
(in-suite base-suite)

(test inversion-sequence/hand
  (is (equalp #() (perm-to-inv #())))
  (is (equalp #() (inv-to-perm #())))
  (is (equalp #(0) (perm-to-inv #(0))))
  (is (equalp #(0) (inv-to-perm #(0))))
  (is (equalp #(0 0) (perm-to-inv #(0 1))))
  (is (equalp #(0 1) (inv-to-perm #(0 0))))
  (is (equalp #(0 1) (perm-to-inv #(1 0))))
  (is (equalp #(1 0) (inv-to-perm #(0 1))))
  (is (equalp #(0 1 0 0 4 5 1) (perm-to-inv #(3 2 4 6 1 0 5))))
  (is (equalp #(3 2 4 6 1 0 5) (inv-to-perm #(0 1 0 0 4 5 1)))))

(test inversion-sequence/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (loop for len from 0 to 100
          for perm = (let ((tmp (make-array len :element-type 'fixnum)))
                       (dotimes (i len tmp)
                         (setf (aref tmp i) i)))
          do (dotimes (_ 10)
               (shuffle! perm)
               (is (equalp perm (inv-to-perm (perm-to-inv perm))))))))
