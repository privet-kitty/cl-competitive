(defpackage :cp/test/persistent-vector
  (:use :cl :fiveam :cp/persistent-vector)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/persistent-vector)
(in-suite base-suite)

(test persistent-vector/random
  ;; random test
  (let ((state (sb-ext:seed-random-state 0)))
    (finishes
      (dotimes (_ 200)
        (let* ((duration 200)
               (length (+ 1 (random 100 state)))
               (master (make-array (list duration length) :initial-element 0))
               (pvs (make-array duration :initial-element nil)))
          (dotimes (time duration)
            (let ((value (- (random 200 state) 100))
                  (index (random length)))
              (setf (aref master time index) value
                    (aref pvs time) (pv-assoc (aref pvs time) index value))))
          (dotimes (time duration)
            (dotimes (index length)
              (assert (= (pv-ref (aref pvs time) index)
                         (aref master time index))))))))))
