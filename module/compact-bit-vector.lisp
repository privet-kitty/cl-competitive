(defpackage :cp/compact-bit-vector
  (:use :cl)
  (:export #:compact-bit-vector #:make-compact-bit-vector! #:cbv-storage #:cbv-blocks
           #:cbv-ref #:cbv-count #:cbv-rank #:cbv-select)
  (:import-from #:sb-vm #:n-word-bits)
  (:documentation "Provides two-layer compact bit vector."))
(in-package :cp/compact-bit-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= n-word-bits 64)))

(deftype uint () '(integer 0 #.most-positive-fixnum))

(defstruct (compact-bit-vector (:constructor %make-cbv (storage blocks))
                               (:conc-name cbv-)
                               (:copier nil)
                               (:predicate nil))
  (storage nil :type simple-bit-vector)
  (blocks nil :type (simple-array uint (*))))
 
(defun make-compact-bit-vector! (vector)
  "The consequence is undefined when VECTOR is modified after a compact bit
vector is created."
  (declare (optimize (speed 3)))
  (check-type vector simple-bit-vector)
  (let* ((vector (if (zerop (mod (length vector) sb-vm:n-word-bits))
                     vector
                     (let ((tmp (make-array (* n-word-bits
                                               (ceiling (length vector) n-word-bits))
                                            :element-type 'bit
                                            :initial-element 0)))
                       (replace tmp vector))))
         (block-count (floor (length vector) n-word-bits))
         (blocks (make-array (+ 1 block-count) :element-type 'uint :initial-element 0))
         (sum 0))
    (declare (simple-bit-vector vector)
             ((mod #.array-dimension-limit) sum))
    (dotimes (i block-count)
      (setf (aref blocks i) sum)
      (incf sum (logcount (sb-kernel:%vector-raw-bits vector i))))
    (setf (aref blocks block-count) sum)
    (%make-cbv vector blocks)))

(declaim (inline cbv-ref))
(defun cbv-ref (cbv index)
  (sbit (cbv-storage cbv) index))
 
;; NOTE: No error handling.
(declaim (inline cbv-rank)
         (ftype (function * (values (mod #.array-dimension-limit) &optional))
                cbv-rank))
(defun cbv-rank (cbv end)
  "Counts the number of 1's in the range [0, END)."
  (declare ((mod #.array-dimension-limit) end))
  (let* ((storage (cbv-storage cbv))
         (blocks (cbv-blocks cbv))
         (bpos (ash end -6))
         (brem (logand #b111111 end)))
    (+ (aref blocks bpos)
       ;; I put this clause to avoid out-of-bounds access but I'm not sure it's
       ;; mandatory. (It is faster without it.)
       (if (zerop brem) 
           0
           (logcount (ldb (byte brem 0)
                          (sb-kernel:%vector-raw-bits storage bpos)))))))

(declaim (inline cbv-count)
         (ftype (function * (values (mod #.array-dimension-limit) &optional))
                cbv-count))
(defun cbv-count (cbv value end)
  "Counts the number of VALUEs in the range [0, END)"
  (declare (bit value)
           ((mod #.array-dimension-limit) end))
  (let ((count1 (cbv-rank cbv end)))
    (if (= value 1)
        count1
        (- end count1))))

(defun cbv-select (cbv ord)
  "Detects the position of (1-based) ORD-th 1 in CBV. (CBV-SELECT CBV 0) always
returns 0."
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) ord))
  (let* ((storage (cbv-storage cbv))
         (blocks (cbv-blocks cbv))
         (block-size (length blocks)))
    (unless (<= ord (aref blocks (- block-size 1)))
      ;; FIXME: introduce condition class
      (error "There aren't ~W 1's in ~W" ord cbv))
    (labels ((block-bisect (ok ng)
               (declare ((unsigned-byte 32) ok ng))
               (if (<= (- ng ok) 1)
                   ok
                   (let ((mid (ash (+ ok ng) -1)))
                     (if (<= ord (aref blocks mid))
                         (block-bisect ok mid)
                         (block-bisect mid ng))))))
      (let* ((block-idx (block-bisect 0 block-size))
             (ord (- ord (aref blocks block-idx)))
             (word (sb-kernel:%vector-raw-bits storage block-idx)))
        ;; TODO: use PDEP instruction here
        (labels ((pos-bisect (ok ng)
                   (declare ((integer 0 #.n-word-bits) ok ng))
                   (if (<= (- ng ok) 1)
                       ok
                       (let ((mid (ash (+ ok ng) -1)))
                         (if (<= ord (logcount (ldb (byte mid 0) word)))
                             (pos-bisect ok mid)
                             (pos-bisect mid ng))))))
          (let ((pos (pos-bisect 0 n-word-bits)))
            (+ (* n-word-bits block-idx) pos)))))))
