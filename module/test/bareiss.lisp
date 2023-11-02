(defpackage :cp/test/bareiss
  (:use :cl :fiveam :cp/bareiss :cp/copy-array :cp/gemm)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/mod-linear-algebra #:mod-echelon! #:mod-determinant!))
(in-package :cp/test/bareiss)
(in-suite base-suite)

(defconstant +mod+ (+ 7 (expt 10 9)))

(deftype uint () '(integer 0 #.most-positive-fixnum))

(defun %bareiss (matrix &optional ext)
  (bareiss! (copy-array matrix) (when ext (copy-array ext))))

(test bareiss!/hand
  ;; zero-size case
  (is (equalp (%bareiss #2a())
              (make-bareiss :matrix #2a()
                            :ext nil
                            :rank 0
                            :det 1
                            :cols (coerce #() '(simple-array uint (*))))))
  (is (equalp (%bareiss (make-array '(0 2) :initial-element 0)
                        (make-array '(0 3) :initial-element 0))
              (make-bareiss :matrix (make-array '(0 2) :initial-element 0)
                            :ext (make-array '(0 3) :initial-element 0)
                            :rank 0
                            :det 1
                            :cols (coerce #() '(simple-array uint (*))))))
  (is (equalp (%bareiss #2a(() ()) #2a((2) (3)))
              (make-bareiss :matrix #2a(() ())
                            :ext #2a((2) (3))
                            :rank 0
                            :det 1
                            :cols (coerce #() '(simple-array uint (*))))))
  ;; one-size case
  (is (equalp (%bareiss #2a((0 0 0)) #2a((3)))
              (make-bareiss :matrix #2a((0 0 0))
                            :ext #2a((3))
                            :rank 0
                            :det 0
                            :cols (coerce #() '(simple-array uint (*))))))
  (is (equalp (%bareiss #2a((3 1)) #2a((4 5)))
              (make-bareiss :matrix #2a((3 1))
                            :ext #2a((4 5))
                            :rank 1
                            :det 3
                            :cols (coerce #(0) '(simple-array uint (*))))))
  (is (equalp (%bareiss #2a((4) (0) (0)) #2a((8 12) (0 0) (0 0)))
              (make-bareiss :matrix #2a((4) (0) (0))
                            :ext #2a((8 12) (0 0) (0 0))
                            :rank 1
                            :det 4
                            :cols (coerce #(0) '(simple-array uint (*))))))
  (is (equalp (%bareiss #2a((4) (0) (1)) #2a((8 12) (0 0) (2 3)))
              (make-bareiss :matrix #2a((4) (0) (0))
                            :ext #2a((8 12) (0 0) (0 0))
                            :rank 1
                            :det 4
                            :cols (coerce #(0) '(simple-array uint (*))))))
  (is (not (equalp (%bareiss #2a((4) (0) (1)) #2a((8 12) (0 0) (2 4)))
                   (make-bareiss :matrix #2a((4) (0) (0))
                                 :ext #2a((8 12) (0 0) (0 0))
                                 :rank 1
                                 :det 4
                                 :cols (coerce #(0) '(simple-array uint (*)))))))
  ;; example from https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process
  (is (equalp (%bareiss #2a((3 1) (2 2)) #2a((5) (6)))
              (make-bareiss :matrix #2a((3 1) (0 4))
                            :ext #2a((5) (8))
                            :rank 2
                            :det 4
                            :cols (coerce #(0 1) '(simple-array uint (*))))))
  ;; example from https://ja.wikipedia.org/wiki/%E3%82%AC%E3%82%A6%E3%82%B9%E3%81%AE%E6%B6%88%E5%8E%BB%E6%B3%95
  (is (equalp (%bareiss #2a((1 3 1) (1 1 -1) (3 11 5)) #2a((9) (1) (35)))
              (make-bareiss :matrix #2a((1 3 1) (0 -2 -2) (0 0 0))
                            :ext #2a((9) (-8) (0))
                            :rank 2
                            :det 0
                            :cols (coerce #(0 1) '(simple-array uint (*))))))
  ;; linearly dependent case
  (is (equalp (%bareiss #2a((3 1 2) (9 3 6)) #2a((5) (15)))
              (make-bareiss :matrix #2a((3 1 2) (0 0 0))
                            :ext #2a((5) (0))
                            :rank 1
                            :det 0
                            :cols (coerce #(0) '(simple-array uint (*))))))
  ;; row full rank case
  (is (equalp (%bareiss #2a((1 1 1) (1 -1 2)) #2a((5 8) (5 4)))
              (make-bareiss :matrix #2a((1 1 1) (0 -2 1))
                            :ext #2a((5 8) (0 -4))
                            :rank 2
                            :det -2
                            :cols (coerce #(0 1) '(simple-array uint (*))))))
  ;; column full rank case
  (is (equalp (%bareiss #2a((1 1) (1 -1) (1 2)) #2a((3) (-1) (5)))
              (make-bareiss :matrix #2a((1 1) (0 -2) (0 0))
                            :ext #2a((3) (-4) (0))
                            :rank 2
                            :det -2
                            :cols (coerce #(0 1) '(simple-array uint (*)))))))

(defun calc-rank (mat)
  (declare (optimize (speed 3))
           ((simple-array * (* *)) mat))
  (destructuring-bind (m n) (array-dimensions  mat)
    (let ((tmp (make-array (list m n) :element-type '(unsigned-byte 31))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref tmp i j) (mod (aref mat i j) +mod+))))
      (nth-value 1 (mod-echelon! tmp +mod+)))))

(defun calc-det (mat cols)
  (declare (optimize (speed 3))
           ((simple-array fixnum (* *)) mat)
           ((simple-array (integer 0 #.most-positive-fixnum) (*)) cols))
  (destructuring-bind (m n) (array-dimensions mat)
    (assert (<= m n))
    (unless (= m (length cols))
      (return-from calc-det 0))
    (let ((rec-mat (make-array (list m m) :element-type '(unsigned-byte 31))))
      (dotimes (i m)
        (dotimes (k m)
          (setf (aref rec-mat i k) (mod (aref mat i (aref cols k)) +mod+))))
      (mod-determinant! rec-mat +mod+))))

(test bareiss!/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (mag '(5 50))
      (loop for trial below 50000
            for m = (+ 1 (random 8))
            for n = (+ 1 (random 8))
            do (let ((mat (make-array (list m n) :element-type 'fixnum)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref mat i j) (- (random (* 2 mag)) mag))))
                 (let* ((bareiss (bareiss! (copy-array mat)))
                        (det (bareiss-det bareiss))
                        (rank (bareiss-rank bareiss))
                        (cols (bareiss-cols bareiss))
                        (trimat (bareiss-matrix bareiss)))
                   (is (= rank (calc-rank mat)))
                   (when (<= m n)
                     (is (= (mod det +mod+) (calc-det mat cols))))
                   (if (= rank (min m n))
                       (is-false (zerop det))
                       (is-true (zerop det)))
                   ;; TODO: better test for column-full-rank case
                   (is (= rank (length cols)))
                   (dotimes (i (- rank 1))
                     (is (< (aref cols i) (aref cols (+ i 1)))))))))))

(test %solve-nonsingular-linear-system!/hand
  ;; zero-size case
  (is (equalp (%solve-nonsingular-linear-system! (copy-array #2a()) (copy-array #2a()))
              #2a()))
  (is (equalp (%solve-nonsingular-linear-system!
               (copy-array #2a()) (make-array '(0 3) :initial-element 0))
              (make-array '(0 3) :initial-element 0)))
  ;; one-size case
  (is (equalp (%solve-nonsingular-linear-system!
               (copy-array #2a((3))) (copy-array #2a((3 6 9))))
              #2a((1 2 3))))
  (is (null (%solve-nonsingular-linear-system!
             (copy-array #2a((3))) (copy-array #2a((3 6 10))))))
  ;; example from https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process
  (is (equalp (%solve-nonsingular-linear-system!
               (copy-array #2a((3 1) (2 2)))
               (copy-array #2a((5) (6))))
              #2a((1) (2)))))

(test %solve-nonsingular-linear-system!/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (mag '(5 50))
      (loop with trial = 0
            while (< trial 30000)
            for m = (+ 1 (random 7))
            for n = (+ 1 (random 7))
            do (let ((a (make-array (list m m) :element-type 'fixnum))
                     (x (make-array (list m n) :element-type 'fixnum)))
                 (dotimes (i m)
                   (dotimes (j m)
                     (setf (aref a i j) (- (random (* 2 mag)) mag))))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref x i j) (- (random (* 2 mag)) mag))))
                 (when (= m (calc-rank a))
                   (let* ((ax (gemm a x))
                          (x-restored (%solve-nonsingular-linear-system!
                                       (copy-array a) (copy-array ax))))
                     (is-false (null x-restored))
                     (when x-restored
                       (is-true (loop for k below (* m n)
                                      always (= (row-major-aref x k)
                                                (row-major-aref x-restored k)))))
                     (incf trial))))))))
