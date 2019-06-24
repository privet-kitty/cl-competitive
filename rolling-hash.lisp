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

(declaim (inline rhash-query)
         (ftype (function * (values (unsigned-byte 32) &optional)) rhash-query))
(defun rhash-query (rhash l r)
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

(defun rhash-get-lcp (rhash1 start1 rhash2 start2)
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) start1 start2))
  (assert (= (rhash-divisor rhash1) (rhash-divisor rhash2)))
  (assert (and (< start1 (length (rhash-cumul rhash1)))
               (< start2 (length (rhash-cumul rhash2)))))
  (let ((max-length (min (- (length (rhash-cumul rhash1)) start1 1)
                         (- (length (rhash-cumul rhash2)) start2 1))))
    (declare (optimize (safety 0)))
    (if (= (rhash-query rhash1 start1 (+ start1 max-length))
           (rhash-query rhash2 start2 (+ start2 max-length)))
        max-length
        (labels ((bisect (ok ng)
                   (declare ((integer 0 #.most-positive-fixnum) ok ng))
                   (if (<= (- ng ok) 1)
                       ok
                       (let ((mid (ash (+ ng ok) -1)))
                         (if (= (rhash-query rhash1 start1 (+ start1 mid))
                                (rhash-query rhash2 start2 (+ start2 mid)))
                             (bisect mid ng)
                             (bisect ok mid))))))
          (bisect 0 max-length)))))

