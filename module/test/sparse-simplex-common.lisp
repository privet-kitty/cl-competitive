(defpackage :cp/test/sparse-simplex-common
  (:use :cl :fiveam :cp/sparse-simplex-common :cp/csc :cp/test/nearly-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/sparse-simplex-common)
(in-suite base-suite)

(test tmat-times-vec!
  (let* ((vec (make-sparse-vector-from #(1d0 0d0 2d0 0d0 -1d0)))
         (tmat (make-csc-from-array #2a((1d0 2d0 3d0 4d0 7d0)
                                        (6d0 7d0 8d0 9d0 10d0)
                                        (11d0 12d0 13d0 14d0 15d0)
                                        (16d0 17d0 18d0 19d00 20d0)
                                        (21d0 22d0 23d0 24d0 25d0)
                                        (100d0 0d0 100d0 1d0 1d0))))
         (res (make-sparse-vector 5))
         (basic-flag (coerce '(-1 -2 -4 -5 0 -3) '(simple-array fixnum (*))))
         (dense (sparse-vector-to-dense (tmat-times-vec! tmat vec basic-flag res))))
    (is (nearly-equalp 1d-8 #(0d0 12d0 299d0 22d0 32d0) dense)))
  (is (equalp #()
              (sparse-vector-to-dense
               (tmat-times-vec! (make-csc-from-array #2a())
                                (make-sparse-vector 0)
                                (make-array 0 :element-type 'fixnum)
                                (make-sparse-vector 0))))))
