;;;
;;; Bit-reversal operation
;;; Reference: https://stackoverflow.com/questions/746171/efficient-algorithm-for-bit-reversal-from-msb-lsb-to-lsb-msb-in-c
;;;

(declaim ((simple-array (unsigned-byte 16) (65536)) *bit-reverse-table*))
(defparameter *bit-reverse-table*
  (make-array 65536 :element-type '(unsigned-byte 16)))

(dotimes (idx (length *bit-reverse-table*))
  (let ((x idx))
    (setq x (logior (ash (logand x #xaaaa) -1)
                    (ash (logand x #x5555) 1)))
    (setq x (logior (ash (logand x #xcccc) -2)
                    (ash (logand x #x3333) 2)))
    (setq x (logior (ash (logand x #xf0f0) -4)
                    (ash (logand x #x0f0f) 4)))
    (setq x (logior (ash x -8) (ldb (byte 16 0) (ash x 8))))
    (setf (aref *bit-reverse-table* idx) x)))

(declaim (inline logreverse))
(defun logreverse (x size)
  "Returns the bit-reversal in the range [0, SIZE) of X."
  (declare ((unsigned-byte 64) x)
           ((integer 0 64) size))
  (let ((table *bit-reverse-table*))
    (ash (logior
          (ash (aref table (logand x #xffff)) 48)
          (ash (aref table (logand (ash x -16) #xffff)) 32)
          (ash (aref table (logand (ash x -32) #xffff)) 16)
          (aref table (logand (ash x -48) #xffff)))
         (- size 64))))

;; Below is a somewhat slower but space efficient bit-reversal
;; (declaim (inline logreverse))
;; (defun logreverse (x size)
;;   "Returns the bit-reversal in the range [0, SIZE) of X."
;;   (declare ((unsigned-byte 64) x)
;;            ((integer 0 64) size))
;;   (if (<= size 8)
;;       (ash (mod (logand (* x #x0202020202) #x010884422010) 1023)
;;            (- size 8))
;;       (progn
;;         (setq x (logior (ash (logand x #xaaaaaaaaaaaaaaaa) -1)
;;                         (ash (logand x #x5555555555555555) 1)))
;;         (setq x (logior (ash (logand x #xcccccccccccccccc) -2)
;;                         (ash (logand x #x3333333333333333) 2)))
;;         (setq x (logior (ash (logand x #xf0f0f0f0f0f0f0f0) -4)
;;                         (ash (logand x #x0f0f0f0f0f0f0f0f) 4)))
;;         (setq x (logior (ash (logand x #xff00ff00ff00ff00) -8)
;;                         (ash (logand x #x00ff00ff00ff00ff) 8)))
;;         (setq x (logior (ash (logand x #xffff0000ffff0000) -16)
;;                         (ash (logand x #x0000ffff0000ffff) 16)))
;;         (ash (logior (ash x -32) (ldb (byte 64 0) (ash x 32)))
;;              (- size 64)))))

(defun bench (sample)
  (declare (fixnum sample)
           (optimize (speed 3) (safety 0)))
  (let ((res 0))
    (declare (fixnum res))
    (dotimes (i sample)
      (setf res
            (logxor res (ldb (byte 1 0) (logreverse (if (oddp i) #x0123456789abcde #xedcba9876543210) 64)))))))
