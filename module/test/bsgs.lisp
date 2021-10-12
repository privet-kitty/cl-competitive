(defpackage :cp/test/bsgs
  (:use :cl :fiveam :cp/bsgs :cp/eratosthenes :cp/mod-log :cp/gemm)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/bsgs)
(in-suite base-suite)

(test bsgs-group/random
  (let ((*test-dribble* nil)
        (ps (make-prime-sequence 10000)))
    (loop for p of-type (unsigned-byte 31) across ps
          do (dotimes (_ 5)
               (dolist (from-zero '(nil t))
                 (let* ((x (+ 1 (random (- p 1))))
                        (y (random p))
                        (res1 (mod-log x y p :from-zero from-zero))
                        (res2 (bsgs-group x
                                          (lambda (x y)
                                            (declare ((unsigned-byte 31) x y))
                                            (mod (* x y) p))
                                          (lambda (x y)
                                            (declare ((unsigned-byte 31) x y))
                                            (mod (* x y) p))
                                          1 y
                                          p
                                          :from-zero from-zero)))
                   (is (eql res1 res2))))))))

(defun compose (f1 f2)
  (declare (optimize (speed 3))
           ((simple-array (unsigned-byte 31) (*)) f1 f2))
  (let ((res (make-array (length f1) :element-type '(unsigned-byte 31))))
    (dotimes (i (length f1) res)
      (setf (aref res i) (aref f1 (aref f2 i))))))

(test bsgs-semigroup/random-map
  (let ((*test-dribble* nil))
    (loop for len from 1 to 100
          do (dotimes (_ 100)
               (let ((x (random len))
                     (y (random len))
                     (f (make-array len :element-type '(unsigned-byte 31))))
                 (dotimes (i len)
                   (setf (aref f i) (random len)))
                 (let ((res1 (loop with x = x
                                   for i below len
                                   when (= x y)
                                   do (return i)
                                   do (setq x (aref f x))))
                       (res2 (bsgs-semigroup
                              f
                              (lambda (f x)
                                (declare ((simple-array (unsigned-byte 31) (*)) f))
                                (aref f x))
                              #'compose
                              x y len)))
                   (is (eql res1 res2))))))))

(defun %gemm (mat1 mat2 p)
  (declare (optimize (speed 3))
           ((unsigned-byte 31) p)
           ((simple-array (unsigned-byte 31) (* *)) mat1 mat2))
  (gemm mat1 mat2 :op+ (lambda (x y) (declare ((unsigned-byte 31) x y))
                         (mod (+ x y) p))
                  :op* (lambda (x y) (declare ((unsigned-byte 31) x y))
                         (mod (* x y) p))))

(defun %gemv (mat vec p)
  (declare (optimize (speed 3))
           ((unsigned-byte 31) p)
           ((simple-array (unsigned-byte 31) (* *)) mat)
           ((simple-array (unsigned-byte 31) (*)) vec))
  (gemv mat vec
        :op+ (lambda (x y) (declare ((unsigned-byte 31) x y))
               (mod (+ x y) p))
        :op* (lambda (x y) (declare ((unsigned-byte 31) x y))
               (mod (* x y) p))))

(defun %naive-cycle-length (mat vec p)
  (declare (optimize (speed 3))
           ((unsigned-byte 31) p)
           ((simple-array (unsigned-byte 31) (* *)) mat)
           ((simple-array (unsigned-byte 31) (*)) vec))
  (let ((table (make-hash-table :test #'equalp)))
    (loop for i of-type (integer 0 #.most-positive-fixnum) from 0
          for prev = (gethash vec table)
          when prev
          do (return (values prev (- i prev)))
          do (setf (gethash vec table) i)
             (setq vec (%gemv mat vec p)))))

(defun %naive-dlp (mat lhs rhs p)
  (declare (optimize (speed 3))
           ((unsigned-byte 31) p)
           ((simple-array (unsigned-byte 31) (* *)) mat)
           ((simple-array (unsigned-byte 31) (*)) lhs rhs))
  (loop for i below (* p p)
        when (equalp lhs rhs)
        do (return i)
        do (setq lhs (%gemv mat lhs p))))

(defun %fast-cycle-length (mat vec p)
  (declare (optimize (speed 3))
           ((unsigned-byte 31) p)
           ((simple-array (unsigned-byte 31) (* *)) mat)
           ((simple-array (unsigned-byte 31) (*)) vec))
  (let* ((fbase-len (integer-length (sb-int:power-of-two-ceiling (* p p))))
         (fbase (make-fbase mat (lambda (a b) (%gemm a b p)) fbase-len))
         (len (%cycle-length fbase
                             mat
                             (lambda (a b) (%gemv a b p))
                             (lambda (a b) (%gemm a b p))
                             vec
                             (* p p)
                             :test #'equalp))
         (start (%cycle-start fbase len mat
                              (lambda (a b) (%gemv a b p))
                              (lambda (a b) (%gemm a b p))
                              vec
                              (* p p)
                              :test #'equalp)))
    (values start len)))

(defun %fast-dlp (mat lhs rhs p)
  (bsgs-semigroup mat
                  (lambda (a b) (%gemv a b p))
                  (lambda (a b) (%gemm a b p))
                  lhs rhs
                  (* p p)
                  :test #'equalp))

(test bsgs-semigroup/random-matrix
  (let ((*test-dribble* nil))
    (dotimes (_ 5000)
      (let ((mat (make-array '(2 2) :element-type '(unsigned-byte 31) :initial-element 0))
            (lhs (make-array 2 :element-type '(unsigned-byte 31) :initial-element 0))
            (rhs (make-array 2 :element-type '(unsigned-byte 31) :initial-element 0))
            (p (+ 1 (random 20))))
        (dotimes (i 2)
          (setf (aref lhs i) (random p)
                (aref rhs i) (random p))
          (dotimes (j 2)
            (setf (aref mat i j) (random p))))
        (multiple-value-bind (start1 len1) (%fast-cycle-length mat lhs p)
          (multiple-value-bind (start2 len2) (%naive-cycle-length mat lhs p)
            (is (eql start1 start2))
            (is (eql len1 len2))))
        (let ((res1 (%fast-dlp mat lhs rhs p))
              (res2 (%naive-dlp mat lhs rhs p)))
          (is (eql res1 res2)))))))
