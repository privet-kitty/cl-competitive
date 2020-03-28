;;;
;;; Primality test (Miller-Rabin)
;;;

;; Tuned for 64-bit SBCL ((INTEGER 0 #.MOST-POSITIVE-FIXNUM) == (UNSIGNED-BYTE
;; 62))

(defun %strong-probable-prime-p (n base)
  (declare (optimize (speed 3))
           ((unsigned-byte 62) n base))
  (or (= n base) ; KLUDGE: is it really approriate to put this form here?
      (let ((d (floor (- n 1) (logand (- n 1) (- 1 n)))))
        (labels ((mod-power (base power)
                   (declare ((unsigned-byte 62) base power)
                            #+sbcl (values (unsigned-byte 62) &optional))
                   (cond ((zerop power) 1)
                         ((evenp power)
                          (mod-power (mod (* base base) n) (ash power -1)))
                         (t (mod (* base (mod-power base (- power 1))) n)))))
          (let ((y (mod-power base d)))
            (declare ((unsigned-byte 62) y))
            (or (= y 1)
                (= y (- n 1))
                (let ((s (- (integer-length (logand (- n 1) (- 1 n))) 1)))
                  (loop repeat (- s 1)
                        do (setq y (mod (* y y) n))
                           (when (<= y 1) (return nil))
                           (when (= y (- n 1)) (return t))))))))))

;; https://primes.utm.edu/prove/prove2_3.html
;; TODO: more efficient SPRP
(defun prime-p (n)
  (declare ((unsigned-byte 62) n))
  (cond ((<= n 1) nil)
        ((evenp n) (= n 2))
        ((< n 4759123141)
         (loop for base in '(2 7 61)
               always (%strong-probable-prime-p n base)))
        ((< n 2152302898747)
         (loop for base in '(2 3 5 7 11)
               always (%strong-probable-prime-p n base)))
        ;; NOTE: branches below are not tested
        ((< n 341550071728321)
         (loop for base in '(2 3 5 7 11 13 17)
               always (%strong-probable-prime-p n base)))
        (t
         (loop for base in '(2 3 5 7 11 13 17 19 23 29)
               always (%strong-probable-prime-p n base)))))
