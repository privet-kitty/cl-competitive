(defpackage :cp/test/iterset
  (:use :cl :fiveam :cp/iterset :cp/bisect)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/iterset)
(in-suite base-suite)

(defun insert (vector key)
  (declare (optimize (speed 3))
           ((simple-array fixnum (*)) vector)
           (fixnum key))
  (let ((pos (bisect-left vector key)))
    (concatenate '(simple-array fixnum (*))
                 (subseq vector 0 pos)
                 (vector key)
                 (subseq vector pos))))

(test iterset/hand
  (let ((iterset (make-iterset)))
    (iterset-insert iterset 0)
    (let ((node (iterset-first iterset)))
      (is (= 0 (node-key node)))
      (is (null (node-prev node)))
      (is (null (node-next node)))
      (iterset-insert iterset -10)
      (is (= 0 (node-key node)))
      (is (= -10 (node-key (node-prev node))))
      (is (null (node-next node)))
      (iterset-insert iterset 10)
      (is (= 0 (node-key node)))
      (is (= -10 (node-key (node-prev node))))
      (is (= 10 (node-key (node-next node))))
      (setq node (node-prev node))
      (is (= -10 (node-key node)))
      (is (null (node-prev node)))
      (is (= 0 (node-key (node-next node))))
      (iterset-insert iterset -5)
      (is (= -10 (node-key node)))
      (is (null (node-prev node)))
      (is (= -5 (node-key (node-next node)))))))

(test iterset/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dotimes (_ 80)
      (let* ((pos 0)
             (vector (make-array 1 :element-type 'fixnum
                                   :initial-element (random most-positive-fixnum)))
             (iterset (make-iterset)))
        (declare ((simple-array fixnum (*)) vector))
        (iterset-insert iterset (aref vector pos))
        (let ((node (iterset-first iterset)))
          (is (= (node-key node) (aref vector pos)))
          (dotimes (_ 80)
            (let ((key (random most-positive-fixnum)))
              (unless (find key vector)
                (iterset-insert iterset key)
                (setq vector (insert vector key)
                      pos (bisect-left vector (node-key node)))))
            (dotimes (_ 5)
              (if (zerop (random 2))
                  (if (node-prev node)
                      (setq node (node-prev node)
                            pos (- pos 1))
                      (is (zerop pos)))
                  (if (node-next node)
                      (setq node (node-next node)
                            pos (+ pos 1))
                      (is (= (+ pos 1) (length vector)))))
              (is (= (node-key node) (aref vector pos))))))))))
