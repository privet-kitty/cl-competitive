;;;
;;; 2D rolling hash (32-bit)
;;;

(defpackage :cp/2d-rolling-hash
  (:use :cl)
  (:export #:make-rhash2d #:rhash2d-matrix-hash))
(in-package :cp/2d-rolling-hash)

;; This table consists of pairs of primes less than 2^32 and the random
;; primitive roots modulo them larger than 10^9. We randomly choose a pair and
;; adopt the prime as modulus and the primitive root as base.
(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (declaim ((simple-array (unsigned-byte 32) (100)) *moduli-table* *base-table*))
  (defparameter *moduli-table*
    (make-array 100 :element-type '(unsigned-byte 32)
                    :initial-contents '(4294967291 4294967279 4294967231 4294967197 4294967189 4294967161 4294967143
4294967111 4294967087 4294967029 4294966997 4294966981 4294966943 4294966927
4294966909 4294966877 4294966829 4294966813 4294966769 4294966667 4294966661
4294966657 4294966651 4294966639 4294966619 4294966591 4294966583 4294966553
4294966477 4294966447 4294966441 4294966427 4294966373 4294966367 4294966337
4294966297 4294966243 4294966237 4294966231 4294966217 4294966187 4294966177
4294966163 4294966153 4294966129 4294966121 4294966099 4294966087 4294966073
4294966043 4294966007 4294966001 4294965977 4294965971 4294965967 4294965949
4294965937 4294965911 4294965887 4294965847 4294965841 4294965839 4294965821
4294965793 4294965767 4294965757 4294965737 4294965733 4294965721 4294965691
4294965683 4294965679 4294965673 4294965671 4294965659 4294965641 4294965617
4294965613 4294965601 4294965581 4294965529 4294965487 4294965461 4294965457
4294965413 4294965383 4294965361 4294965347 4294965331 4294965313 4294965307
4294965263 4294965251 4294965229 4294965203 4294965193 4294965161 4294965151
4294965137 4294965131)))
  (defparameter *base-table*
    (make-array 100 :element-type '(unsigned-byte 32)
                    :initial-contents '(2247433164 2139372809 2609807693 2343117402 3096734379 2843084587 3022604264
3725165355 1310011850 3271696819 3710639434 4215251668 2971116345 1291563131
2125491020 1561805191 3225016848 4113447491 3038900010 3636011022 2479454799
1990556577 2661169605 3088947962 1926120766 4105365454 4171519129 2043031086
1810297004 1391329364 3781496513 3524912702 2014602604 3608350570 2970210993
4041943368 3843309586 1048071792 2527337250 4207345339 3745845437 3780181639
1843103547 1471147023 2925746977 2571168523 1911322179 2533579172 2577088289
3082429185 3636817029 3517246253 2141978180 2042755180 1656982819 2160802626
3780428251 1987808226 3883058504 1973235694 3022446019 3414211768 2747857698
1121927034 2368051231 1585372041 2942376489 1760007658 1731546725 3503068146
3139298718 3516795165 3838735245 3491469147 2711077678 1556341778 2556545397
1528640652 1183190693 2870857999 3301248018 4114187491 2653041143 1757252280
3464064684 1655297946 4217483675 2809928527 2757106005 3401026515 2587333052
1757998238 1398188339 4075136024 2780360736 2566409334 2544620190 1754492744
2431582005 1565067593)))

  (defun %choose-moduli (&optional mod1 mod2 base1 base2)
    "Chooses two appropriate pairs of moduli and bases."
    (declare ((or null (unsigned-byte 32)) mod1 mod2 base1 base2))
    (let* ((rand1 (random (length *moduli-table*)))
           ;; avoid the same modulus
           (rand2 (loop (let ((tmp (random (length *moduli-table*))))
                          (unless (= tmp rand1)
                            (return tmp))))))
      (if mod1
          (progn
            #+sbcl (assert (sb-int:positive-primep mod1))
            (setq base1 (or base1 (+ 1 (random (- mod1 1))))))
          (progn
            (setq mod1 (or mod1 (aref *moduli-table* rand1)))
            (if base1
                (assert (<= 1 base1 (- mod1 1)))
                (setq base1 (aref *base-table* rand1)))))
      (if mod2
          (progn
            #+sbcl (assert (sb-int:positive-primep mod2))
            (setq base2 (or base2 (+ 1 (random (- mod2 1))))))
          (progn
            (setq mod2 (or mod2 (aref *moduli-table* rand2)))
            (if base2
                (assert (<= 1 base2 (- mod2 1)))
                (setq base2 (aref *base-table* rand2))))))
    (values mod1 mod2 base1 base2))

  (unless (and (boundp '+rhash2d-mod1+)
               (boundp '+rhash2d-mod2+)
               (boundp '+rhash2d-base1+)
               (boundp '+rhash2d-base2+))
    (multiple-value-bind (mod1 mod2 base1 base2) (%choose-moduli)
      (defconstant +rhash2d-mod1+ mod1)
      (defconstant +rhash2d-mod2+ mod2)
      (defconstant +rhash2d-base1+ base1)
      (defconstant +rhash2d-base2+ base2))) )

(defun make-rhash2d (matrix h w &key (key #'identity))
  "Returns the table of the hash value of each subrectangle of size H * W on
MATRIX modulo +RHASH2D-MOD1+ and +RHASH2D-MOD2+. KEY is applied to each element
of MATRIX prior to computing the hash value.

KEY := FUNCTION returning FIXNUM
"
  (declare (optimize (speed 3))
           ((array * (* *)) matrix)
           ((integer 0 #.most-positive-fixnum) h w)
           (function key))
  (labels ((power (base exp mod)
             (declare ((unsigned-byte 32) base exp mod))
             (let ((res 1))
               (declare ((unsigned-byte 32) res))
               (dotimes (i exp res)
                 (setq res (mod (* res base) mod)))))
           (get-cell (i j) ; Returns MATRIX[i][j] as (unsigned-byte 32).
             (declare ((integer 0 #.most-positive-fixnum) i j))
             (the (unsigned-byte 32)
                  (mod (the fixnum (funcall key (aref matrix i j))) +rhash2d-mod1+))))
    (destructuring-bind (src-h src-w) (array-dimensions matrix)
      (declare ((integer 0 #.most-positive-fixnum) src-h src-w))
      (assert (and (<= h src-h) (<= w src-w)))
      (let* ((table-h (+ 1 (- src-h h)))
             (table-w (+ 1 (- src-w w)))
             (tmp-table (make-array (list src-h table-w)
                                    :element-type '(unsigned-byte 32)))
             (table (make-array (list table-h table-w)
                                :element-type '(unsigned-byte 32)))
             (coef-row (power +rhash2d-base1+ w +rhash2d-mod1+))
             (coef-col (power +rhash2d-base2+ h +rhash2d-mod2+)))
        (declare ((integer 0 #.most-positive-fixnum) table-h table-w))
        ;; compute hash values in the horizontal direction
        (dotimes (i src-h)
          (let ((val 0))
            (declare ((unsigned-byte 32) val))
            (dotimes (j w)
              (setq val (mod (+ (* val +rhash2d-base1+) (get-cell i j))
                             +rhash2d-mod1+)))
            (dotimes (j table-w)
              (setf (aref tmp-table i j) val)
              (when (< j (- src-w w))
                (setq val (mod (+ (mod (* val +rhash2d-base1+) +rhash2d-mod1+)
                                  (- +rhash2d-mod1+ (mod (* coef-row (get-cell i j))
                                                         +rhash2d-mod1+))
                                  (get-cell i (+ j w)))
                               +rhash2d-mod1+))))))
        ;; compute hash values in the vertical direction
        (dotimes (j table-w)
          (let ((val 0))
            (declare ((unsigned-byte 32) val))
            (dotimes (i h)
              (setq val (mod (+ (* val +rhash2d-base2+) (aref tmp-table i j))
                             +rhash2d-mod2+)))
            (dotimes (i table-h)
              (setf (aref table i j) val)
              (when (< i (- src-h h))
                (setq val (mod (+ (mod (* val +rhash2d-base2+) +rhash2d-mod2+)
                                  (- +rhash2d-mod2+ (mod (* coef-col (aref tmp-table i j))
                                                         +rhash2d-mod2+))
                                  (aref tmp-table (the fixnum (+ i h)) j))
                               +rhash2d-mod2+))))))
        table))))

(declaim (ftype (function * (values (unsigned-byte 32) &optional)) rhash2d-matrix-hash)
         (inline rhash-matrix-hash))
(defun rhash2d-matrix-hash (matrix &key (key #'identity))
  "Returns the hash code of MATRIX."
  (declare (optimize (speed 3))
           ((array * (* *)) matrix)
           (function key))
  (destructuring-bind (h w) (array-dimensions matrix)
    (declare ((integer 0 #.most-positive-fixnum) h w))
    (let ((res 0))
      (declare ((unsigned-byte 32) res))
      (labels ((get-cell (i j)     ; Returns MATRIX[i][j] as (unsigned-byte 32).
                 (declare ((integer 0 #.most-positive-fixnum) i j))
                 (the (unsigned-byte 32)
                      (mod (the fixnum (funcall key (aref matrix i j))) +rhash2d-mod1+))))
        ;; compute hash values in the horizontal direction
        (dotimes (i h)
          (let ((row-val 0))
            (declare ((unsigned-byte 32) row-val))
            (dotimes (j w)
              (setq row-val (mod (+ (* row-val +rhash2d-base1+) (get-cell i j))
                                 +rhash2d-mod1+)))
            (setq res (mod (+ (* res +rhash2d-base2+) row-val) +rhash2d-mod2+))))
        res))))
