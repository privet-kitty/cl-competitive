(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../min-cost-flow.lisp")
  (load "../jonker-volgenant.lisp"))

(use-package :test-util)

;; We rely on SSP to verify Jonker-Volgenant
(defstruct (matching-ssp (:constructor make-matching-ssp
                             (size1 size2
                              &aux (size (+ 2 size1 size2))
                                   (source (+ size1 size2))
                                   (sink (+ 1 size1 size2))
                                   (graph (make-array size :element-type 'list :initial-element nil))))
                         (:conc-name ssp-))
  (graph nil :type (simple-array list (*)))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (source 0 :type (integer 0 #.most-positive-fixnum))
  (sink 0 :type (integer 0 #.most-positive-fixnum)))

(defun ssp-add-edge (ssp from to cost capacity)
  (add-edge! from to capacity cost (ssp-graph ssp)))

(defun ssp-compute (ssp flow)
  (min-cost-flow! (ssp-source ssp) (ssp-sink ssp) flow (ssp-graph ssp)))

(with-test (:name jonker-volgenant/manual)
  (let ((lap (make-lap 3 4 t)))
    (lap-add-edge lap 0 0 5)
    (lap-add-edge lap 0 2 3)
    (lap-add-edge lap 1 0 4)
    (lap-add-edge lap 1 3 1)
    (lap-add-edge lap 2 1 7)
    (lap-add-edge lap 2 3 2)
    (lap-build lap 3)
    (assert (= 14 (lap-score lap)))
    (assert (equalp (%lap-matching1 lap) #(2 0 1)))
    (assert (equalp (%lap-matching2 lap) (vector 1 2 0 +lap-null-vertex+)))))
