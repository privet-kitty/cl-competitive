(defpackage :cp/test/geometric-sequence
  (:use :cl :fiveam :cp/geometric-sequence :cp/mod-power)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/geometric-sequence)
(in-suite base-suite)

(test geometric-sequence
  (let ((*test-dribble* nil))
    (dotimes (scale 8)
      (dotimes (len 8)
        (dotimes (rate 8)
          (loop for mod from 1 to 10
                for table = (make-geometric-sequence rate len mod)
                do (dotimes (i len)
                     (is (= (mod-power rate i mod) (aref table i))))))))))
