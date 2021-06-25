(defpackage :cp/test/rdtscp
  (:use :cl :fiveam :cp/rdtscp)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/rdtscp)
(in-suite base-suite)

(test read-tsc
  (let ((*test-dribble* nil))
    (dotimes (_ 10000)
      (is (< (read-tsc) (read-tsc))))))
