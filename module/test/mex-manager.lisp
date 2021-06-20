(defpackage :cp/test/mex-manager
  (:use :cl :fiveam :cp/mex-manager)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mex-manager)
(in-suite base-suite)

(test mex-manager/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dolist (default-size '(0 10 100))
      (loop for max from 1 to 100
            for marked = (make-array (+ 1 max) :element-type 'bit :initial-element 0)
            for mm = (make-mex-manager default-size)
            do (dotimes (_ 100)
                 (let ((number (random max)))
                   (setf (aref marked number) 1)
                   (mex-add mm number)
                   (is (= (mex-get mm) (position 0 marked)))))))))
