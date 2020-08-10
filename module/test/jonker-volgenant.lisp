(defpackage :cp/test/jonker-volgenant
  (:use :cl :fiveam :cp/jonker-volgenant :cp/min-cost-flow :cp/ssp)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/jonker-volgenant)
(in-suite base-suite)

(defparameter *state* (sb-ext:seed-random-state 0))

;; We rely on SSP to verify Jonker-Volgenant
(defstruct (matching-ssp (:constructor %make-ssp
                             (size1 size2 &optional maximize
                              &aux (size (+ 2 size1 size2))
                                   (source (+ size1 size2))
                                   (sink (+ 1 size1 size2))
                                   (graph (make-array size :element-type 'list :initial-element nil))))
                         (:conc-name ssp-))
  (graph nil :type (simple-array list (*)))
  (maximize nil :type boolean)
  (size1 0 :type (integer 0 #.most-positive-fixnum))
  (size2 0 :type (integer 0 #.most-positive-fixnum))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (source 0 :type (integer 0 #.most-positive-fixnum))
  (sink 0 :type (integer 0 #.most-positive-fixnum)))

(defun ssp-add-edge (ssp from to cost)
  (let ((cost (if (ssp-maximize ssp) (- cost) cost))
        (size1 (ssp-size1 ssp)))
    (add-cedge (ssp-graph ssp) from (+ size1 to) cost 1)
    ssp))

(defun make-ssp (size1 size2 &optional maximize)
  (let* ((ssp (%make-ssp size1 size2 maximize))
         (graph (ssp-graph ssp))
         (source (ssp-source ssp))
         (sink (ssp-sink ssp)))
    (loop for v below size1
          do (add-cedge graph source v 0 1))
    (loop for v from size1 below (+ size1 size2)
          do (add-cedge graph v sink 0 1))
    ssp))

(defun ssp-compute (ssp flow)
  (let ((res (block proc
               (handler-bind ((not-enough-capacity-error
                                (lambda (c)
                                  (return-from proc (not-enough-capacity-error-score c)))))
                 (min-cost-flow! (ssp-graph ssp) (ssp-source ssp) (ssp-sink ssp) flow)))))
    (if (ssp-maximize ssp)
        (- res)
        res)))

(defun make-random-graph (size1 size2 rate &optional maximize)
  (let ((ssp (make-ssp size1 size2 maximize))
        (lap (make-lap size1 size2 maximize)))
    (dotimes (i size1)
      (dotimes (j size2)
        (when (< (random 1d0 *state*) rate)
          (let ((weight (- (random 30 *state*) 10)))
            (ssp-add-edge ssp i j weight)
            (lap-add-edge lap i j weight)))))
    (values ssp lap)))

(defun %equalp (&rest args)
  (if (cdr args)
      (and (equalp (first args) (second args))
           (apply #'%equalp (cdr args)))
      t))

(test jonker-volgenant/manual
  (let ((lap (make-lap 3 4 t)))
    (lap-add-edge lap 0 0 5)
    (lap-add-edge lap 0 2 3)
    (lap-add-edge lap 1 0 4)
    (lap-add-edge lap 1 3 1)
    (lap-add-edge lap 2 1 7)
    (lap-add-edge lap 2 3 2)
    (lap-build lap 3)
    (is (= 14 (lap-score lap)))
    (is (equalp (%lap-matching1 lap) #(2 0 1)))
    (is (equalp (%lap-matching2 lap) (vector 1 2 0 +lap-null-vertex+))))
  ;; empty case
  (let ((lap1 (make-lap 0 3))
        (lap2 (make-lap 0 3 t))
        (lap3 (make-lap 3 0))
        (lap4 (make-lap 3 0 t))
        (lap5 (make-lap 0 0))
        (lap6 (make-lap 0 0 t)))
    (lap-build lap1 0)
    (lap-build lap2 0)
    (lap-build lap3 0)
    (lap-build lap4 0)
    (lap-build lap5 0)
    (lap-build lap6 0)
    (is (%equalp #()
                 (%lap-matching1 lap1)
                 (%lap-matching1 lap2)
                 (%lap-matching2 lap3)
                 (%lap-matching2 lap4)
                 (%lap-matching1 lap5)
                 (%lap-matching1 lap6)
                 (%lap-matching2 lap5)
                 (%lap-matching2 lap6)))
    (is (%equalp #(-1 -1 -1)
                 (%lap-matching2 lap1)
                 (%lap-matching2 lap2)
                 (%lap-matching1 lap3)
                 (%lap-matching1 lap4)))
    (is (= 0
           (lap-score lap1)
           (lap-score lap2)
           (lap-score lap3)
           (lap-score lap4)
           (lap-score lap5)
           (lap-score lap6))))

  ;; one vertex case
  (let ((lap1 (make-lap 1 1))
        (lap2 (make-lap 1 2 t)))
    (lap-add-edge lap1 0 0 3)
    (lap-build lap1 1)
    (is (= 3 (lap-score lap1)))
    (lap-build lap1 0)
    (is (= 0 (lap-score lap1)))
    (lap-add-edge lap2 0 1 -3)
    (lap-build lap2 1)
    (is (= -3 (lap-score lap2)))
    (is (equalp #(1) (%lap-matching1 lap2)))
    (is (equalp #(-1 0) (%lap-matching2 lap2)))))

(test jonker-volgenant/random-minimize
  ;; size1 = size2
  (finishes
    (dotimes (_ 1000)
      (multiple-value-bind (ssp lap) (make-random-graph 30 30 (random 1d0 *state*))
        (let ((query-size (random 31 *state*)))
          (lap-build lap query-size)
          (assert (= (lap-score lap) (ssp-compute ssp query-size)))))))
  ;; size1 < size2
  (finishes
    (dotimes (_ 1000)
      (multiple-value-bind (ssp lap) (make-random-graph 20 30 (random 1d0 *state*))
        (let ((query-size (random 21 *state*)))
          (lap-build lap query-size)
          (assert (= (lap-score lap) (ssp-compute ssp query-size)))))))
  ;; size1 > size2
  (finishes
    (dotimes (_ 1000)
      (multiple-value-bind (ssp lap) (make-random-graph 30 20 (random 1d0 *state*))
        (let ((query-size (random 21 *state*)))
          (lap-build lap query-size)
          (assert (= (lap-score lap) (ssp-compute ssp query-size))))))))

(test jonker-volgenant/random-maximize
  ;; size1 = size2
  (finishes
    (dotimes (_ 1000)
      (multiple-value-bind (ssp lap) (make-random-graph 30 30 (random 1d0 *state*) t)
        (let ((query-size (random 31 *state*)))
          (lap-build lap query-size)
          (assert (= (lap-score lap) (ssp-compute ssp query-size)))))))
  ;; size1 < size2
  (finishes
    (dotimes (_ 1000)
      (multiple-value-bind (ssp lap) (make-random-graph 20 30 (random 1d0 *state*) t)
        (let ((query-size (random 21 *state*)))
          (lap-build lap query-size)
          (assert (= (lap-score lap) (ssp-compute ssp query-size)))))))
  ;; size1 > size2
  (finishes
    (dotimes (_ 1000)
      (multiple-value-bind (ssp lap) (make-random-graph 30 20 (random 1d0 *state*) t)
        (let ((query-size (random 21 *state*)))
          (lap-build lap query-size)
          (assert (= (lap-score lap) (ssp-compute ssp query-size))))))))
