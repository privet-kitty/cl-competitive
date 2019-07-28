;;;
;;; Rolling hash (32-bit)
;;; NOTE: 32-bit doesn't suffice especially when the strong collision resistance
;;; is required. Better to use 62-bit version instead.
;;;

(defstruct (rhash (:constructor %make-rhash (modulus base cumul powers)))
  (modulus 4294967291 :type (unsigned-byte 32))
  (base 2095716802 :type (unsigned-byte 32))
  (cumul nil :type (simple-array (unsigned-byte 32) (*)))
  (powers nil :type (simple-array (unsigned-byte 32) (*))))

(declaim (inline make-rhash))
(defun make-rhash (vector modulus &key (key #'char-code) base)
  "Returns the table of rolling-hash of VECTOR modulo MODULUS. KEY is applied to
  each element of VECTOR prior to computing the hash value.

MODULUS := unsigned 32-bit prime number
BASE := 1 | 2 | ... | MODULUS - 1
KEY := function returning FIXNUM"
  (declare (vector vector)
           ((unsigned-byte 32) modulus)
           ((or null (unsigned-byte 32)) base)
           (function key))
  (assert (sb-int:positive-primep modulus))
  (let* ((base (or base (+ 1 (random (- modulus 1)))))
         (size (length vector))
         (cumul (make-array (+ 1 size) :element-type '(unsigned-byte 32)))
         (powers (make-array (+ 1 size) :element-type '(unsigned-byte 32))))
    (assert (<= 1 base (- modulus 1)))
    (setf (aref powers 0) 1)
    (dotimes (i size)
      (setf (aref powers (+ i 1))
            (mod (* (aref powers i) base) modulus))
      (let ((sum (+ (mod (* (aref cumul i) base) modulus)
                    (mod (the fixnum (funcall key (aref vector i))) modulus))))
        (setf (aref cumul (+ i 1))
              (if (> sum modulus)
                  (- sum modulus)
                  sum))))
    (%make-rhash modulus base cumul powers)))

(declaim (inline rhash-query)
         (ftype (function * (values (unsigned-byte 32) &optional)) rhash-query))
(defun rhash-query (rhash l r)
  "Returns the hash value of the interval [L, R)."
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (<= l r))
  (let ((cumul (rhash-cumul rhash))
        (powers (rhash-powers rhash))
        (modulus (rhash-modulus rhash)))
    (let ((res (+ (aref cumul r)
                  (- modulus (mod (* (aref cumul l) (aref powers (- r l))) modulus)))))
      (if (> res modulus)
          (- res modulus)
          res))))

(declaim (inline rhash-concat))
(defun rhash-concat (rhash hash1 hash2 hash2-length)
  (declare ((unsigned-byte 32) hash1 hash2)
           ((integer 0 #.most-positive-fixnum) hash2-length))
  (let* ((modulus (rhash-modulus rhash)))
    (mod (+ hash2
            (* hash1 (aref (rhash-powers rhash) hash2-length)))
         modulus)))

(declaim (ftype (function * (values (unsigned-byte 32) &optional)) rhash-vector-hash)
         (inline rhash-vector-hash))
(defun rhash-vector-hash (rhash vector &key (key #'char-code))
  "Returns the hash code of VECTOR w.r.t. the moduli and bases of RHASH."
  (declare (vector vector))
  (let* ((mod (rhash-modulus rhash))
         (base (rhash-base rhash))
         (size (length vector))
         (result 0))
    (declare ((unsigned-byte 32) result))
    (dotimes (i size)
      ;; (2^32-1) * (2^32-1) + (2^32-1) < 2^64
      (setq result (mod (+ (* base result)
                           (the (unsigned-byte 32)
                                (mod (the fixnum (funcall key (aref vector i))) mod)))
                        mod)))
    result))

(defun rhash-get-lcp (rhash1 start1 rhash2 start2)
  (declare (optimize (speed 3))
           ((mod #.most-positive-fixnum) start1 start2))
  (assert (= (rhash-modulus rhash1) (rhash-modulus rhash2)))
  (assert (and (< start1 (length (rhash-cumul rhash1)))
               (< start2 (length (rhash-cumul rhash2)))))
  (let ((max-length (min (- (length (rhash-cumul rhash1)) start1 1)
                         (- (length (rhash-cumul rhash2)) start2 1))))
    (declare (optimize (safety 0)))
    (labels ((bisect (ok ng)
               (declare ((integer 0 #.most-positive-fixnum) ok ng))
               (if (<= (- ng ok) 1)
                   ok
                   (let ((mid (ash (+ ng ok) -1)))
                     (if (= (rhash-query rhash1 start1 (+ start1 mid))
                            (rhash-query rhash2 start2 (+ start2 mid)))
                         (bisect mid ng)
                         (bisect ok mid))))))
      (bisect 0 (+ 1 max-length)))))
