(declaim (inline tzcount))
(defun tzcount (x)
  "Is equivalent to TZCNT operation. Returns the number of trailing zero
bits. Note: (TZCOUNT 0) = -1."
  (- (integer-length (logand x (- x))) 1))

;; The following code works too though it is somewhat slow on AtCoder as
;; LOGCOUNT is not transformed to POPCNT on SBCL 1.1.14.

;; (declaim (inline tzcount2))
;; (defun tzcount2 (x)
;;   (logcount (- (logand x (- x)) 1)))

;; (defun bench (size)
;;   (declare (fixnum size)
;;            (optimize (speed 3) (safety 0)))
;;   (let ((seed (sb-ext:seed-random-state 0)))
;;     (time (loop repeat size
;;                 sum (tzcount (random #xffffffff seed)) of-type fixnum))))
