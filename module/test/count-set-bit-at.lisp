(defpackage :cp/test/count-set-bit-at
  (:use :cl :fiveam :cp/count-set-bit-at)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/count-set-bit-at)
(in-suite base-suite)

(test count-set-bit-at
  (let ((*test-dribble* nil))
    (dotimes (pos 13)
      (let ((sum 0))
        (dotimes (x 3000)
          (when (logbitp pos x)
            (incf sum))
          (is (= sum (count-set-bit-at x pos))))))))
