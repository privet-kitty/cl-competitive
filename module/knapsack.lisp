(defpackage :cp/knapsack
  (:use :cl)
  (:export #:knapsack-merge))
(in-package :cp/knapsack)

;; TODO: add document and test
(declaim (inline knapsack-merge))
(defun knapsack-merge (weights values update-weight update-value
                       &key (order-w #'<) (order-v #'<))
  "Merges new item to the set of possible sum of weights and values.

Note:
- WEIGHTS [VALUES] must be strictly monotone w.r.t. ORDER-W [ORDER-V].
- UPDATE-WEIGHT [UPDATE-VALUE] must (at least weakly) monotonically update the
argument w.r.t. ORDER-W [ORDER-V]."
  (declare (vector weights values)
           (function update-weight update-value order-w order-v))
  (assert (= (length weights) (length values)))
  (let* ((new-ws (make-array (* 2 (length weights)) :element-type 'fixnum))
         (new-vs (make-array (* 2 (length values)) :element-type 'fixnum))
         (len (length values))
         (pos1 0)
         (pos2 0)
         (end 0))
    (declare ((simple-array fixnum (*)) new-ws new-vs)
             ((mod #.array-dimension-limit) pos1 pos2 end))
    (labels ((%append (new-w new-v)
               (declare (fixnum new-w new-v))
               (cond
                 ;; prev W < new W & prev V < new V
                 ((or (zerop end)
                      (and (funcall order-w (aref new-ws (- end 1)) new-w)
                           (funcall order-v (aref new-vs (- end 1)) new-v)))
                  (setf (aref new-ws end) new-w
                        (aref new-vs end) new-v
                        end (+ end 1)))
                 ;; prev W == new W & prev V < new V
                 ((and (not (funcall order-w new-w (aref new-ws (- end 1))))
                       (funcall order-v (aref new-vs (- end 1)) new-v))
                  (setf (aref new-ws (- end 1)) new-w
                        (aref new-vs (- end 1)) new-v)))))
      (loop (when (= pos1 len)
              (loop for pos from pos2 below len
                    do (%append (funcall update-weight (aref weights pos))
                                (funcall update-value (aref values pos))))
              (return))
            (when (= pos2 len)
              (loop for pos from pos1 below len
                    do (%append (aref weights pos) (aref values pos)))
              (return))
            (let ((w1 (aref weights pos1))
                  (v1 (aref values pos1))
                  (w2 (funcall update-weight (aref weights pos2)))
                  (v2 (funcall update-value (aref values pos2))))
              (declare (fixnum v1 v2 w1 w2))
              (cond ((funcall order-w w1 w2)
                     (%append w1 v1)
                     (incf pos1))
                    ((funcall order-w w2 w1)
                     (%append w2 v2)
                     (incf pos2))
                    (t ; (= w1 (+ w2 new-weight))
                     (if (funcall order-v v1 v2)
                         (%append w1 v2)
                         (%append w1 v1))
                     (incf pos1)
                     (incf pos2))))))
    (values (adjust-array new-ws end)
            (adjust-array new-vs end))))
