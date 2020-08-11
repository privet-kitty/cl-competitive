;;;
;;; Succinct bit vector (select: O(log(n)))
;;;

(defpackage :cp/succinct-bit-vector
  (:use :cl)
  (:export #:succint-bit-vector #:make-sucbv! #:sucbv-ref #:sucbv-rank #:sucbv-select))
(in-package :cp/succinct-bit-vector)

;; NOTE: compact-bit-vector will be more efficient than this module.

(defconstant +chunk-width+ (* 64 16))
;; This constant cannot be changed as the current implementation depends on the
;; assumption: +BLOCK-WIDTH+ is equal to the word size.
(defconstant +block-width+ 64)
(defconstant +block-number+ (floor +chunk-width+ +block-width+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (zerop (mod +chunk-width+ +block-width+)))
  (assert (= sb-vm:n-word-bits 64)))

(defstruct (succinct-bit-vector (:constructor %make-sucbv (storage chunks blocks))
                                (:conc-name sucbv-)
                                (:copier nil))
  (storage nil :type simple-bit-vector)
  (chunks nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (blocks nil :type (simple-array (unsigned-byte 16) (* *))))

(defun make-sucbv! (vector)
  "The consequence is undefined when VECTOR is modified after a succinct bit
vector is created."
  (declare (optimize (speed 3)))
  (check-type vector simple-bit-vector)
  (let* ((vector (if (zerop (mod (length vector) +chunk-width+))
                     vector
                     (adjust-array vector
                                   (* +chunk-width+ (ceiling (length vector) +chunk-width+))
                                   :initial-element 0)))
         (len (length vector))
         (chunk-count (floor len +chunk-width+))
         (chunks (make-array (+ 1 chunk-count)
                             :element-type '(integer 0 #.most-positive-fixnum)
                             :initial-element 0))
         (blocks (make-array (list (+ 1 chunk-count) +block-number+)
                             :element-type '(unsigned-byte 16)
                             :initial-element 0))
         (sum 0))
    (declare (simple-bit-vector vector)
             ((integer 0 #.most-positive-fixnum) sum))
    (dotimes (i chunk-count)
      (setf (aref chunks i) sum)
      (let ((block-sum 0))
        (declare ((integer 0 #.most-positive-fixnum) block-sum))
        (dotimes (j +block-number+)
          (setf (aref blocks i j) block-sum)
          (incf block-sum
                (logcount (sb-kernel:%vector-raw-bits vector (+ (* i +block-number+) j)))))
        (incf sum block-sum)))
    (setf (aref chunks chunk-count) sum)
    (%make-sucbv vector chunks blocks)))

(declaim (inline sucbv-ref))
(defun sucbv-ref (sucbv index)
  (sbit (sucbv-storage sucbv) index))

;; NOTE: No error handling.
(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) sucbv-rank))
(defun sucbv-rank (sucbv end)
  "Counts the number of 1's in the range [0, END)."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) end))
  (let ((storage (sucbv-storage sucbv))
        (chunks (sucbv-chunks sucbv))
        (blocks (sucbv-blocks sucbv)))
    (multiple-value-bind (cpos crem) (floor end +chunk-width+)
      (multiple-value-bind (bpos brem) (floor crem +block-width+)
        (let ((csum (aref chunks cpos))
              (bsum (aref blocks cpos bpos))
              (wordpos (floor end 64)))
          (+ csum
             bsum
             (if (zerop brem) ; avoid out-of-bounds access
                 0
                 (logcount (ldb (byte brem 0)
                                (sb-kernel:%vector-raw-bits storage wordpos))))))))))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) sucbv-count))
(defun sucbv-count (sucbv value end)
  "Counts the number of VALUEs in the range [0, END)"
  (declare (optimize (speed 3))
           (bit value)
           ((integer 0 #.most-positive-fixnum) end))
  (let ((count1 (sucbv-rank sucbv end)))
    (if (= value 1)
        count1
        (- end count1))))

(defun sucbv-select (sucbv num)
  "Detects the position of (1-based) NUM-th 1 in SUCBV. (SUCBV-SELECT 0) always
returns 0."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) num))
  (let* ((storage (sucbv-storage sucbv))
         (chunks (sucbv-chunks sucbv))
         (blocks (sucbv-blocks sucbv))
         (chunk-size (length chunks)))
    (unless (<= num (aref chunks (- chunk-size 1)))
      ;; FIXME: introduce condition class
      (error "~&There aren't ~W 1's in ~W" num sucbv))
    (labels ((chunk-bisect (ok ng)
               (declare ((unsigned-byte 32) ok ng))
               (if (<= (- ng ok) 1)
                   ok
                   (let ((mid (ash (+ ok ng) -1)))
                     (if (<= num (aref chunks mid))
                         (chunk-bisect ok mid)
                         (chunk-bisect mid ng))))))
      (let* ((chunk-idx (chunk-bisect 0 chunk-size))
             (num (- num (aref chunks chunk-idx))))
        (labels ((block-bisect (ok ng)
                   (declare ((unsigned-byte 32) ok ng))
                   (if (<= (- ng ok) 1)
                       ok
                       (let ((mid (ash (+ ok ng) -1)))
                         (if (<= num (aref blocks chunk-idx mid))
                             (block-bisect ok mid)
                             (block-bisect mid ng))))))
          (let* ((block-idx (block-bisect 0 +block-number+))
                 (num (- num (aref blocks chunk-idx block-idx)))
                 (word-pos (+ block-idx (* chunk-idx +block-number+)))
                 (word (sb-kernel:%vector-raw-bits storage word-pos)))
            (labels ((pos-bisect (ok ng)
                       (declare ((integer 0 64) ok ng))
                       (if (<= (- ng ok) 1)
                           ok
                           (let ((mid (ash (+ ok ng) -1)))
                             (if (<= num (logcount (ldb (byte mid 0) word)))
                                 (pos-bisect ok mid)
                                 (pos-bisect mid ng))))))
              (let ((pos (pos-bisect 0 64)))
                (+ (* 64 word-pos) pos)))))))))
