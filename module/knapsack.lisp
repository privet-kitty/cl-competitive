(defpackage :cp/knapsack
  (:use :cl)
  (:export #:knapsack-merge))
(in-package :cp/knapsack)

;; TODO: add document and test
(defun knapsack-merge (values weights new-value new-weight)
  "Merges new item to the set of possible sum of values and weights. VALUES and
WEIGHTS must be strictly increasing.

VALUES [WEIGHTS] := increasing sequence of sum of values [weights]"
  (declare (vector values weights)
           ((integer 0 #.most-positive-fixnum) new-value new-weight))
  (assert (= (length values) (length weights)))
  (let* ((new-vs (make-array (* 2 (length values)) :element-type 'fixnum))
         (new-ws (make-array (* 2 (length weights)) :element-type 'fixnum))
         (len (length values))
         (pos1 0)
         (pos2 0)
         (end 0)
         (current-w most-negative-fixnum)
         (current-v most-negative-fixnum))
    (declare ((simple-array fixnum (*)) new-vs new-ws)
             ((integer 0 #.array-dimension-limit) pos1 pos2 end)
             (fixnum current-w current-v))
    (loop (when (= pos1 len)
            (loop for pos from pos2 below len
                  for v2 of-type (integer 0 #.most-positive-fixnum) = (aref values pos)
                  for w2 of-type (integer 0 #.most-positive-fixnum) = (aref weights pos)
                  when (and (> (+ v2 new-value) current-v)
                            (> (+ w2 new-weight) current-w))
                  do (setf (aref new-vs end) (+ v2 new-value)
                           (aref new-ws end) (+ w2 new-weight)
                           end (+ end 1)
                           current-w (+ w2 new-weight)
                           current-v (+ v2 new-value)))
            (return))
          (assert (>= pos1 pos2))
          (let ((v1 (aref values pos1))
                (w1 (aref weights pos1))
                (v2 (aref values pos2))
                (w2 (aref weights pos2)))
            (declare ((integer 0 #.most-positive-fixnum) v1 v2 w1 w2))
            (cond ((< w1 (+ w2 new-weight))
                   (when (and (> w1 current-w)
                              (> v1 current-v))
                     (setf (aref new-vs end) v1
                           (aref new-ws end) w1
                           end (+ end 1)
                           current-w w1
                           current-v v1))
                   (incf pos1))
                  ((> w1 (+ w2 new-weight))
                   (when (and (> (+ w2 new-weight) current-w)
                              (> (+ v2 new-value) current-v))
                     (setf (aref new-vs end) (+ v2 new-value)
                           (aref new-ws end) (+ w2 new-weight)
                           end (+ end 1)
                           current-w (+ w2 new-weight)
                           current-v (+ v2 new-value)))
                   (incf pos2))
                  (t ;; (= w1 (+ w2 new-weight))
                   (let ((max-v (max v1 (+ v2 new-value))))
                     (declare ((integer 0 #.most-positive-fixnum) max-v))
                     (when (and (> w1 current-w)
                                (> max-v current-v))
                       (setf (aref new-vs end) max-v
                             (aref new-ws end) w1
                             end (+ end 1)
                             current-w w1
                             current-v max-v))
                     (incf pos1)
                     (incf pos2))))))
    (values (adjust-array new-vs end)
            (adjust-array new-ws end))))
