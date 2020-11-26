(defpackage :cp/test/mod-power-table
  (:use :cl :fiveam :cp/mod-power-table :cp/mod-power)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-power-table)
(in-suite base-suite)

(test mod-power-table
  (let ((*test-dribble* nil))
    (dotimes (len 8)
      (dotimes (base 8)
        (loop for mod from 1 to 10
              for table = (make-mod-power-table base len mod)
              do (dotimes (i len)
                   (is (= (mod-power base i mod) (aref table i)))))))))
