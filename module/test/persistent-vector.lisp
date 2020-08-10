(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../persistent-vector.lisp"))

(use-package :test-util)

(defparameter *state* (sb-ext:seed-random-state 0))

(with-test (:name persistent-vector)
  ;; random test
  (dotimes (_ 200)
    (let* ((duration 200)
           (length (+ 1 (random 100 *state*)))
           (master (make-array (list duration length) :initial-element 0))
           (pvs (make-array duration :initial-element nil)))
      (dotimes (time duration)
        (let ((value (- (random 200 *state*) 100))
              (index (random length)))
          (setf (aref master time index) value
                (aref pvs time) (pv-assoc (aref pvs time) index value))))
      (dotimes (time duration)
        (dotimes (index length)
          (assert (= (pv-ref (aref pvs time) index)
                     (aref master time index))))))))
