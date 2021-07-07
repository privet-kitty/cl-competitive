(defpackage :cp/test/incremental-lp
  (:use :cl :fiveam :cp/incremental-lp :cp/two-phase-simplex :cp/lp-test-tool
   :cp/test/nearly-equal :cp/gaussian)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/incremental-lp)
(in-suite base-suite)

(defun check* (a b c)
  (declare (optimize (speed 3))
           ((simple-array double-float (* *)) a)
           ((simple-array double-float (*)) b c))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((lp (make-lp (copy-seq c))))
      (dotimes (i m)
        (let* ((tmp-a (make-array (list i n) :element-type 'double-float))
               (tmp-b (subseq b 0 i))
               (tmp-c (copy-seq c)))
          (replace (sb-ext:array-storage-vector tmp-a) (sb-ext:array-storage-vector a))
          (multiple-value-bind (res1 prim1 dual1) (dual-primal! tmp-a tmp-b tmp-c)
            (multiple-value-bind (res2 prim2 dual2) (lp-solve! lp)
              (if (numberp res1)
                  (progn
                    
                    (is (nearly= 1d-8 res1 res2))
                    (is (nearly-equal 1d-8 (coerce prim1 'list) (coerce prim2 'list))))
                  (progn
                    (is (eql res1 res2))
                    (is (not (or prim1 dual1 prim2 dual2)))))))
          (let ((tmp-row (make-array n :element-type 'double-float)))
            (dotimes (j n)
              (setf (aref tmp-row j) (aref a i j)))
            (lp-add-row! lp tmp-row (aref b i))
            (setq lp (lp-render lp))))))))

(test incremental-lp/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (_ 200)
      (multiple-value-bind (a b c) (make-random-instance 10d0 :non-negative-b t)
        (check* a b c)))
    (dotimes (_ 200)
      (multiple-value-bind (a b c) (make-random-instance 10d0 )
        (check* a b c)))))
