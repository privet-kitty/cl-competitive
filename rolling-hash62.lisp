;;;
;;; Rolling hash (62-bit)
;;;

;; Reference: https://www.mii.lt/olympiads_in_informatics/pdf/INFOL119.pdf
;; TODO: get the hash value of a given string or char
;; TODO: automatically choose a large prime and its (large) primitive root.
(defstruct (rhash (:constructor %make-rhash (modulus1 cumul1 powers1 modulus2 cumul2 powers2)))
  ;; lower 31-bit value
  (modulus1 1000000007 :type (unsigned-byte 31))
  (cumul1 nil :type (simple-array (unsigned-byte 31) (*)))
  (powers1 nil :type (simple-array (unsigned-byte 31) (*)))
  ;; upper 31-bit value
  (modulus2 1000000009 :type (unsigned-byte 31))
  (cumul2 nil :type (simple-array (unsigned-byte 31) (*)))
  (powers2 nil :type (simple-array (unsigned-byte 31) (*))))

(defun make-rhash (vector modulus1 modulus2 &key (key #'char-code) base1 base2)
  "Returns the table of rolling-hash of VECTOR modulo MODULUS1 and MODULUS2. KEY
is applied to each element of VECTOR prior to computing the hash value.

MODULUS[1|2] := unsigned 31-bit prime number
BASE1 := 1 | 2 | ... | MODULUS1 - 1
BASE2 := 1 | 2 | ... | MODULUS2 - 1
KEY := function returning FIXNUM"
  (declare (optimize (speed 3))
           (vector vector)
           ((unsigned-byte 31) modulus1 modulus2)
           ((or null (unsigned-byte 31)) base1 base2)
           (function key))
  (assert (and (sb-int:positive-primep modulus1)
               (sb-int:positive-primep modulus2)))
  (let* ((base1 (or base1 (+ 1 (random (- modulus1 1)))))
         (base2 (or base2 (+ 1 (random (- modulus2 1)))))
         (size (length vector))
         (cumul1 (make-array (+ 1 size) :element-type '(unsigned-byte 31)))
         (powers1 (make-array (+ 1 size) :element-type '(unsigned-byte 31)))
         (cumul2 (make-array (+ 1 size) :element-type '(unsigned-byte 31)))
         (powers2 (make-array (+ 1 size) :element-type '(unsigned-byte 31))))
    (declare ((unsigned-byte 31) base1 base2))
    (assert (and (<= 1 base1 (- modulus1 1))
                 (<= 1 base2 (- modulus2 1))))
    (setf (aref powers1 0) 1
          (aref powers2 0) 1)
    (dotimes (i size)
      (setf (aref powers1 (+ i 1))
            (mod (* (aref powers1 i) base1) modulus1)
            (aref powers2 (+ i 1))
            (mod (* (aref powers2 i) base2) modulus2))
      (let ((sum1 (+ (mod (* base1 (aref cumul1 i)) modulus1)
                     (mod (the fixnum (funcall key (aref vector i))) modulus1)))
            (sum2 (+ (mod (* base2 (aref cumul2 i)) modulus2)
                     (mod (the fixnum (funcall key (aref vector i))) modulus2))))
        (setf (aref cumul1 (+ i 1)) (if (> sum1 modulus1)
                                        (- sum1 modulus1)
                                        sum1)
              (aref cumul2 (+ i 1)) (if (> sum2 modulus2)
                                        (- sum2 modulus2)
                                        sum2))))
    (%make-rhash modulus1 cumul1 powers1 modulus2 cumul2 powers2)))

(declaim (inline rhash-query)
         (ftype (function * (values (unsigned-byte 62) &optional)) rhash-query))
(defun rhash-query (rhash l r)
  "Returns the hash value of the interval [L, R)."
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (<= l r))
  (let ((cumul1 (rhash-cumul1 rhash))
        (powers1 (rhash-powers1 rhash))
        (modulus1 (rhash-modulus1 rhash))
        (cumul2 (rhash-cumul2 rhash))
        (powers2 (rhash-powers2 rhash))
        (modulus2 (rhash-modulus2 rhash)))
    (let ((lower (+ (aref cumul1 r)
                    (- modulus1 (mod (* (aref cumul1 l) (aref powers1 (- r l))) modulus1))))
          (upper (+ (aref cumul2 r)
                    (- modulus2 (mod (* (aref cumul2 l) (aref powers2 (- r l))) modulus2)))))
      (let ((lower (if (> lower modulus1) (- lower modulus1) lower))
            (upper (if (> upper modulus2) (- upper modulus2) upper)))
        (declare ((unsigned-byte 31) lower upper))
        (dpb upper (byte 31 31) lower)))))

(declaim (inline rhash-concat))
(defun rhash-concat (rhash hash1 hash2 length2)
  "Returns the hash value of the concatenated sequence.

HASH1 := hash value of the first sequence
HASH2 := hash value of the second sequence
LENGTH2 := length of the second sequence."
  (declare ((unsigned-byte 62) hash1 hash2)
           ((integer 0 #.most-positive-fixnum) length2))
  (let* ((hash1-lower (ldb (byte 31 0) hash1))
         (hash1-upper (ldb (byte 31 31) hash1))
         (hash2-lower (ldb (byte 31 0) hash2))
         (hash2-upper (ldb (byte 31 31) hash2))
         (modulus1 (rhash-modulus1 rhash))
         (modulus2 (rhash-modulus2 rhash))
         (res-lower (mod (+ hash2-lower
                            (mod (* hash1-lower
                                    (aref (rhash-powers1 rhash) length2))
                                 modulus1))
                         modulus1))
         (res-upper (mod (+ hash2-upper
                            (mod (* hash1-upper
                                    (aref (rhash-powers2 rhash) length2))
                                 modulus2))
                         modulus2)))
    (declare ((unsigned-byte 31) res-lower res-upper))
    (dpb res-upper (byte 31 31) res-lower)))

(defun rhash-get-lcp (rhash1 start1 rhash2 start2)
  "Returns the length of the longest common prefix of two suffixes which begin
at START1 and START2."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) start1 start2))
  (assert (and (= (rhash-modulus1 rhash1) (rhash-modulus1 rhash2))
               (= (rhash-modulus2 rhash1) (rhash-modulus2 rhash2))))
  (assert (and (< start1 (length (rhash-cumul1 rhash1)))
               (< start2 (length (rhash-cumul1 rhash2)))))
  (let ((max-length (min (- (length (rhash-cumul1 rhash1)) start1 1)
                         (- (length (rhash-cumul1 rhash2)) start2 1))))
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

