;;;
;;; Rolling hash (62-bit)
;;;

;; Reference: https://www.mii.lt/olympiads_in_informatics/pdf/INFOL119.pdf
;; TODO: get the hash value of a given string or char

(defstruct (rhash (:constructor %make-rhash (mod1 cumul1 powers1 mod2 cumul2 powers2)))
  ;; lower 31-bit value
  (mod1 1000000007 :type (unsigned-byte 31))
  (cumul1 nil :type (simple-array (unsigned-byte 31) (*)))
  (powers1 nil :type (simple-array (unsigned-byte 31) (*)))
  ;; upper 31-bit value
  (mod2 1000000009 :type (unsigned-byte 31))
  (cumul2 nil :type (simple-array (unsigned-byte 31) (*)))
  (powers2 nil :type (simple-array (unsigned-byte 31) (*))))

;; This table consists of pairs of a prime less than 2^31 and the corresponding
;; largest primitive root. We randomly choose a pair and adopt the prime as
;; modulus and the primitive root as base.
(declaim ((simple-array (cons (unsigned-byte 31) (unsigned-byte 31)) (100)) *moduli-table*))
(defparameter *moduli-table*
  (make-array 100 :element-type '(cons (unsigned-byte 31) (unsigned-byte 31))
                  :initial-contents '((2147483647 . 2147483634) (2147483629 . 2147483627) (2147483587 . 2147483583) (2147483579 . 2147483576) (2147483563 . 2147483550) (2147483549 . 2147483546) (2147483543 . 2147483541) (2147483497 . 2147483492) (2147483489 . 2147483486) (2147483477 . 2147483475) (2147483423 . 2147483421) (2147483399 . 2147483397) (2147483353 . 2147483348) (2147483323 . 2147483289) (2147483269 . 2147483267) (2147483249 . 2147483246) (2147483237 . 2147483235) (2147483179 . 2147483175) (2147483171 . 2147483168) (2147483137 . 2147483127) (2147483123 . 2147483120) (2147483077 . 2147483072) (2147483069 . 2147483067) (2147483059 . 2147483055) (2147483053 . 2147483048) (2147483033 . 2147483030) (2147483029 . 2147483027) (2147482951 . 2147482944) (2147482949 . 2147482947) (2147482943 . 2147482941) (2147482937 . 2147482934) (2147482921 . 2147482900) (2147482877 . 2147482875) (2147482873 . 2147482862) (2147482867 . 2147482863) (2147482859 . 2147482856) (2147482819 . 2147482815) (2147482817 . 2147482814) (2147482811 . 2147482807) (2147482801 . 2147482779) (2147482763 . 2147482760) (2147482739 . 2147482736) (2147482697 . 2147482694) (2147482693 . 2147482691) (2147482681 . 2147482668) (2147482663 . 2147482661) (2147482661 . 2147482659) (2147482621 . 2147482619) (2147482591 . 2147482589) (2147482583 . 2147482581) (2147482577 . 2147482574) (2147482507 . 2147482494) (2147482501 . 2147482499) (2147482481 . 2147482478) (2147482417 . 2147482412) (2147482409 . 2147482406) (2147482367 . 2147482364) (2147482361 . 2147482358) (2147482349 . 2147482347) (2147482343 . 2147482341) (2147482327 . 2147482325) (2147482291 . 2147482286) (2147482273 . 2147482268) (2147482237 . 2147482235) (2147482231 . 2147482229) (2147482223 . 2147482221) (2147482121 . 2147482109) (2147482093 . 2147482091) (2147482091 . 2147482087) (2147482081 . 2147482070) (2147482063 . 2147482061) (2147482021 . 2147482003) (2147481997 . 2147481995) (2147481967 . 2147481965) (2147481949 . 2147481947) (2147481937 . 2147481932) (2147481907 . 2147481903) (2147481901 . 2147481899) (2147481899 . 2147481896) (2147481893 . 2147481891) (2147481883 . 2147481879) (2147481863 . 2147481861) (2147481827 . 2147481824) (2147481811 . 2147481807) (2147481797 . 2147481795) (2147481793 . 2147481788) (2147481673 . 2147481668) (2147481629 . 2147481627) (2147481571 . 2147481558) (2147481563 . 2147481560) (2147481529 . 2147481518) (2147481509 . 2147481507) (2147481499 . 2147481495) (2147481491 . 2147481488) (2147481487 . 2147481485) (2147481373 . 2147481371) (2147481367 . 2147481365) (2147481359 . 2147481357) (2147481353 . 2147481350) (2147481337 . 2147481302))))

(defun %choose-moduli (mod1 mod2 base1 base2)
  (declare ((or null (unsigned-byte 31)) mod1 mod2 base1 base2))
  (let* ((rand1 (random (length *moduli-table*)))
         (rand2 (loop (let ((tmp (random (length *moduli-table*))))
                        (unless (= tmp rand1)
                          (return tmp))))))
    (if mod1
        (progn
          (assert (sb-int:positive-primep mod1))
          (setq base1 (or base1 (+ 1 (random (- mod1 1))))))
        (progn
          (setq mod1 (or mod1 (car (aref *moduli-table* rand1))))
          (if base1
              (assert (<= 1 base1 (- mod1 1)))
              (setq base1 (cdr (aref *moduli-table* rand1))))))
    (if mod2
        (progn
          (assert (sb-int:positive-primep mod2))
          (setq base2 (or base2 (+ 1 (random (- mod2 1))))))
        (progn
          (setq mod2 (or mod2 (car (aref *moduli-table* rand2))))
          (if base2
              (assert (<= 1 base2 (- mod2 1)))
              (setq base2 (cdr (aref *moduli-table* rand2)))))))
  (values mod1 mod2 base1 base2))

(defun make-rhash (vector &key (key #'char-code) mod1 mod2 base1 base2)
  "Returns the table of rolling-hash of VECTOR modulo MOD1 and MOD2. KEY
is applied to each element of VECTOR prior to computing the hash value.

MOD[1|2] := unsigned 31-bit prime number
BASE1 := 1 | 2 | ... | MOD1 - 1
BASE2 := 1 | 2 | ... | MOD2 - 1
KEY := function returning FIXNUM"
  (declare (optimize (speed 3))
           (vector vector)
           ((or null (unsigned-byte 31)) mod1 mod2 base1 base2)
           (function key))
  (multiple-value-bind (mod1 mod2 base1 base2) (%choose-moduli mod1 mod2 base1 base2)
    (declare ((unsigned-byte 31) mod1 mod2 base1 base2))
    (let* ((size (length vector))
           (cumul1 (make-array (+ 1 size) :element-type '(unsigned-byte 31)))
           (powers1 (make-array (+ 1 size) :element-type '(unsigned-byte 31)))
           (cumul2 (make-array (+ 1 size) :element-type '(unsigned-byte 31)))
           (powers2 (make-array (+ 1 size) :element-type '(unsigned-byte 31))))
      (setf (aref powers1 0) 1
            (aref powers2 0) 1)
      (dotimes (i size)
        (setf (aref powers1 (+ i 1))
              (mod (* (aref powers1 i) base1) mod1)
              (aref powers2 (+ i 1))
              (mod (* (aref powers2 i) base2) mod2))
        (let ((sum1 (+ (mod (* base1 (aref cumul1 i)) mod1)
                       (mod (the fixnum (funcall key (aref vector i))) mod1)))
              (sum2 (+ (mod (* base2 (aref cumul2 i)) mod2)
                       (mod (the fixnum (funcall key (aref vector i))) mod2))))
          (setf (aref cumul1 (+ i 1)) (if (> sum1 mod1)
                                          (- sum1 mod1)
                                          sum1)
                (aref cumul2 (+ i 1)) (if (> sum2 mod2)
                                          (- sum2 mod2)
                                          sum2))))
      (%make-rhash mod1 cumul1 powers1 mod2 cumul2 powers2))))

(declaim (inline rhash-query)
         (ftype (function * (values (unsigned-byte 62) &optional)) rhash-query))
(defun rhash-query (rhash l r)
  "Returns the hash value of the interval [L, R)."
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (<= l r))
  (let ((cumul1 (rhash-cumul1 rhash))
        (powers1 (rhash-powers1 rhash))
        (mod1 (rhash-mod1 rhash))
        (cumul2 (rhash-cumul2 rhash))
        (powers2 (rhash-powers2 rhash))
        (mod2 (rhash-mod2 rhash)))
    (let ((lower (+ (aref cumul1 r)
                    (- mod1 (mod (* (aref cumul1 l) (aref powers1 (- r l))) mod1))))
          (upper (+ (aref cumul2 r)
                    (- mod2 (mod (* (aref cumul2 l) (aref powers2 (- r l))) mod2)))))
      (let ((lower (if (> lower mod1) (- lower mod1) lower))
            (upper (if (> upper mod2) (- upper mod2) upper)))
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
         (mod1 (rhash-mod1 rhash))
         (mod2 (rhash-mod2 rhash))
         (res-lower (mod (+ hash2-lower
                            (mod (* hash1-lower
                                    (aref (rhash-powers1 rhash) length2))
                                 mod1))
                         mod1))
         (res-upper (mod (+ hash2-upper
                            (mod (* hash1-upper
                                    (aref (rhash-powers2 rhash) length2))
                                 mod2))
                         mod2)))
    (declare ((unsigned-byte 31) res-lower res-upper))
    (dpb res-upper (byte 31 31) res-lower)))

(defun rhash-get-lcp (rhash1 start1 rhash2 start2)
  "Returns the length of the longest common prefix of two suffixes which begin
at START1 and START2."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) start1 start2))
  (assert (and (= (rhash-mod1 rhash1) (rhash-mod1 rhash2))
               (= (rhash-mod2 rhash1) (rhash-mod2 rhash2))))
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

