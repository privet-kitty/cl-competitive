(declaim (inline tzcount))
(defun tzcount (x)
  (- (integer-length (logand x (- x))) 1))

;; (declaim (inline tzcount2))
;; (defun tzcount2 (x)
;;   (logcount (- (logand x (- x)) 1)))

;; (defun bench (size)
;;   (declare (fixnum size)
;;            (optimize (speed 3) (safety 0)))
;;   (let ((seed (sb-ext:seed-random-state 0)))
;;     (time (loop repeat size
;;                 sum (tzcount (random #xffffffff seed)) of-type fixnum))))
