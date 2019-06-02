;;;
;;; Unfinished. Use fft-recursive.lisp instead.
;;;

(deftype fft-float () 'double-float)

(declaim (inline logreverse))
(defun logreverse (x size)
  "Returns the bit-reversal in the range [0, SIZE) of X."
  (declare ((unsigned-byte 64) x)
           ((integer 0 64) size))
  (if (<= size 8)
      (ash (mod (logand (* x #x0202020202) #x010884422010) 1023)
           (- size 8))
      (progn
        (setq x (logior (ash (logand x #xaaaaaaaaaaaaaaaa) -1)
                        (ash (logand x #x5555555555555555) 1)))
        (setq x (logior (ash (logand x #xcccccccccccccccc) -2)
                        (ash (logand x #x3333333333333333) 2)))
        (setq x (logior (ash (logand x #xf0f0f0f0f0f0f0f0) -4)
                        (ash (logand x #x0f0f0f0f0f0f0f0f) 4)))
        (setq x (logior (ash (logand x #xff00ff00ff00ff00) -8)
                        (ash (logand x #x00ff00ff00ff00ff) 8)))
        (setq x (logior (ash (logand x #xffff0000ffff0000) -16)
                        (ash (logand x #x0000ffff0000ffff) 16)))
        (ash (logior (ash x -32)
                     (ldb (byte 64 0) (ash x 32)))
             (- size 64)))))
