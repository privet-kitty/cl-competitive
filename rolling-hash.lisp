;;;
;;; Rolling hash
;;;

;; TODO: handle multiple moduli together
(defstruct (rhash (:constructor %make-rhash (divisor cumul powers)))
  (divisor 1000000007 :type (unsigned-byte 32))
  (cumul nil :type (simple-array (unsigned-byte 32) (*)))
  (powers nil :type (simple-array (unsigned-byte 32) (*))))

(declaim (inline make-rhash))
(defun make-rhash (vector divisor &key (key #'char-code) base)
  "Returns the table of rolling-hash of VECTOR modulo DIVISOR. KEY is applied to
  each element of VECTOR prior to computing the hash value.

DIVISOR := unsigned 32-bit prime number
BASE := 1 | 2 | ... | DIVISOR - 1
KEY := function returning FIXNUM"
  (declare (vector vector)
           ((unsigned-byte 32) divisor)
           ((or null (unsigned-byte 32)) base)
           (function key))
  (assert (sb-int:positive-primep divisor))
  (let* ((base (or base (+ 1 (random (- divisor 1)))))
         (size (length vector))
         (cumul (make-array (+ 1 size) :element-type '(unsigned-byte 32)))
         (powers (make-array (+ 1 size) :element-type '(unsigned-byte 32))))
    (assert (<= 1 base (- divisor 1)))
    (setf (aref powers 0) 1)
    (dotimes (i size)
      (setf (aref powers (+ i 1))
            (mod (* (aref powers i) base) divisor))
      (let ((sum (+ (mod (* (aref cumul i) base) divisor)
                    (mod (the fixnum (funcall key (aref vector i))) divisor))))
        (setf (aref cumul (+ i 1))
              (if (> sum divisor)
                  (- sum divisor)
                  sum))))
    (%make-rhash divisor cumul powers)))

(declaim (inline rhash-get)
         (ftype (function * (values (unsigned-byte 32) &optional)) rhash-get))
(defun rhash-get (rhash l r)
  "Returns the hash value of the interval [L, R)."
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (<= l r))
  (let ((cumul (rhash-cumul rhash))
        (powers (rhash-powers rhash))
        (divisor (rhash-divisor rhash)))
    (let ((res (+ (aref cumul r)
                  (- divisor (mod (* (aref cumul l) (aref powers (- r l))) divisor)))))
      (if (> res divisor)
          (- res divisor)
          res))))

(declaim (inline rhash-concat))
(defun rhash-concat (rhash hash1 hash2 hash2-length)
  (declare ((unsigned-byte 32) hash1 hash2)
           ((integer 0 #.most-positive-fixnum) hash2-length))
  (let* ((divisor (rhash-divisor rhash)))
    (mod (+ hash2
            (mod (* hash1
                    (aref (rhash-powers rhash) hash2-length))
                 divisor))
         divisor)))
