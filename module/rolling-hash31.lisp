;;;
;;; Rolling hash (32-bit)
;;; NOTE: 32-bit doesn't suffice especially when the strong collision resistance
;;; is required. Better to use 62-bit version instead.
;;;

(defpackage :cp/rolling-hash31
  (:use :cl)
  (:export #:*moduli-table* #:*base-table* #:+rhash-mod+ #:+rhash-base+
           #:rhash #:make-rhash #:rhash-vector-hash #:rhash-concat #:rhash-query #:rhash-get-lcp))
(in-package :cp/rolling-hash31)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This table consists of pairs of primes less than 2^31 and the random
  ;; primitive roots modulo them and larger than 10^9. We randomly choose a pair
  ;; and adopt the prime as modulus, and the primitive root as base.
  (declaim ((simple-array (unsigned-byte 31) (100)) *moduli-table* *base-table*))
  (defparameter *moduli-table*
    (make-array 100 :element-type '(unsigned-byte 31)
                    :initial-contents '(2147483647 2147483629 2147483587 2147483579 2147483563 2147483549 2147483543
 2147483497 2147483489 2147483477 2147483423 2147483399 2147483353 2147483323
 2147483269 2147483249 2147483237 2147483179 2147483171 2147483137 2147483123
 2147483077 2147483069 2147483059 2147483053 2147483033 2147483029 2147482951
 2147482949 2147482943 2147482937 2147482921 2147482877 2147482873 2147482867
 2147482859 2147482819 2147482817 2147482811 2147482801 2147482763 2147482739
 2147482697 2147482693 2147482681 2147482663 2147482661 2147482621 2147482591
 2147482583 2147482577 2147482507 2147482501 2147482481 2147482417 2147482409
 2147482367 2147482361 2147482349 2147482343 2147482327 2147482291 2147482273
 2147482237 2147482231 2147482223 2147482121 2147482093 2147482091 2147482081
 2147482063 2147482021 2147481997 2147481967 2147481949 2147481937 2147481907
 2147481901 2147481899 2147481893 2147481883 2147481863 2147481827 2147481811
 2147481797 2147481793 2147481673 2147481629 2147481571 2147481563 2147481529
 2147481509 2147481499 2147481491 2147481487 2147481373 2147481367 2147481359
 2147481353 2147481337)))
  (defparameter *base-table*
    (make-array 100 :element-type '(unsigned-byte 31)
                    :initial-contents '(1059428526 2090066834 1772913519 1695158082 1516083910 1622025757 1248368302
 1894391153 2094976878 1193495823 1783230399 1520742486 1748395380 1703688443
 2138630366 1942049269 2066548889 1890950855 1480056952 1792721876 1092797280
 1204851872 1035383130 1002272185 1319736653 1980774767 1748793187 1866963602
 1200445534 1732959733 1214706585 1957228822 1479411729 1323155655 1052714514
 1989821027 1163834549 1095622874 2087901566 1670886084 1191975321 2091468260
 1429690292 1116037844 1420457779 1937649612 1552519679 1328604092 2090326292
 1397132095 1316705322 1664351025 1391513321 1851038917 1556301575 1928956735
 1764506480 1449537491 2119470570 1793768237 1831208371 1723755364 1643456516
 1993819805 1419297891 1755252963 1775153034 1388979165 2144586633 1501222238
 1872274033 1143076711 1229125474 1483974015 1997206147 1593231852 1632083893
 1601537043 2012194627 1299923971 1566635240 1814404069 1619988648 2072686565
 2014361572 1213868607 1166967329 1009325840 1306167671 1915239658 1223190075
 1821151471 2037700892 1646950698 1517859810 1099233635 1004913731 1653443892
 1782112665 1018916580)))

  (defun %choose-moduli (&optional mod base)
    (declare ((or null (unsigned-byte 31)) mod base))
    (let* ((rand (random (length *moduli-table*))))
      (if mod
          (progn
            #+sbcl (assert (sb-int:positive-primep mod))
            (setq base (or base (+ 1 (random (- mod 1))))))
          (progn
            (setq mod (or mod (aref *moduli-table* rand)))
            (if base
                (assert (<= 1 base (- mod 1)))
                (setq base (aref *base-table* rand))))))
    (values mod base))
  (unless (and (boundp '+rhash-mod+) (boundp '+rhash-base+))
    (multiple-value-bind (mod base) (%choose-moduli)
      (defconstant +rhash-mod+ mod)
      (defconstant +rhash-base+ base))))

(defstruct (rhash (:constructor %make-rhash (cumul powers))
                  (:copier nil)
                  (:predicate nil))
  (cumul nil :type (simple-array (unsigned-byte 31) (*)))
  (powers nil :type (simple-array (unsigned-byte 31) (*))))

;; KLUDGE: Type derivation of MOD fails in some cases on SBCL. See
;; https://bugs.launchpad.net/sbcl/+bug/1843108
(declaim (notinline %mod))
(defun %mod (number divisor)
  (nth-value 1 (floor number divisor)))

(declaim (inline make-rhash))
(defun make-rhash (vector &key (key #'char-code))
  "Returns the table of rolling-hash of VECTOR modulo +RHASH-MOD+. KEY is applied to
  each element of VECTOR prior to computing the hash value.

KEY := function returning FIXNUM"
  (declare (vector vector)
           (function key))
  (let* ((size (length vector))
         (cumul (make-array (+ 1 size) :element-type '(unsigned-byte 31)))
         (powers (make-array (+ 1 size) :element-type '(unsigned-byte 31))))
    (setf (aref powers 0) 1)
    (dotimes (i size)
      (setf (aref powers (+ i 1))
            (%mod (* (aref powers i) +rhash-base+) +rhash-mod+))
      (let ((sum (+ (%mod (* (aref cumul i) +rhash-base+) +rhash-mod+)
                    (%mod (the fixnum (funcall key (aref vector i))) +rhash-mod+))))
        (setf (aref cumul (+ i 1))
              (if (> sum +rhash-mod+)
                  (- sum +rhash-mod+)
                  sum))))
    (%make-rhash cumul powers)))

(declaim (ftype (function * (values (unsigned-byte 31) &optional)) rhash-vector-hash)
         (inline rhash-vector-hash))
(defun rhash-vector-hash (vector &key (key #'char-code))
  "Returns the hash code of VECTOR w.r.t. the moduli and bases of RHASH."
  (declare (vector vector))
  (let* ((size (length vector))
         (result 0))
    (declare ((unsigned-byte 31) result))
    (dotimes (i size)
      ;; (2^31-1) * (2^31-1) + (2^31-1) < 2^62
      (setq result (%mod (+ (* +rhash-base+ result)
                            (the (unsigned-byte 31)
                                 (%mod (the fixnum (funcall key (aref vector i))) +rhash-mod+)))
                         +rhash-mod+)))
    result))

(declaim (inline rhash-query)
         (ftype (function * (values (unsigned-byte 31) &optional)) rhash-query))
(defun rhash-query (rhash l r)
  "Returns the hash value of the interval [L, R)."
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (<= l r))
  (let ((cumul (rhash-cumul rhash))
        (powers (rhash-powers rhash)))
    (let ((res (+ (aref cumul r)
                  (- +rhash-mod+ (%mod (* (aref cumul l) (aref powers (- r l))) +rhash-mod+)))))
      (if (>= res +rhash-mod+)
          (- res +rhash-mod+)
          res))))

(declaim (inline rhash-concat))
(defun rhash-concat (rhash hash1 hash2 length2)
  (declare ((unsigned-byte 31) hash1 hash2)
           ((integer 0 #.most-positive-fixnum) length2))
  (%mod (+ hash2
           (* hash1 (aref (rhash-powers rhash) length2)))
        +rhash-mod+))

(declaim (ftype (function * (values (unsigned-byte 31) &optional)) rhash-get-lcp))
(defun rhash-get-lcp (rhash1 start1 rhash2 start2)
  (declare (optimize (speed 3))
           ((mod #.most-positive-fixnum) start1 start2))
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
