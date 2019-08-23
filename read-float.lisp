;; This is an efficient float reader specialized for the numbers that can be
;; handled within the range of FIXNUM.
(defun read-simple-float (&optional (in *standard-input*))
  "Reads a fixed point float in the format of *READ-DEFAULT-FLOAT-FORMAT*.

NOTE: two numbers before and after the decimal point must be within (INTEGER 0
#.MOST-POSITIVE-FIXNUM)."
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (sb-impl::ansi-stream-read-byte in nil #.(char-code #\Nul) nil))))
    (let* ((byte 0)
           (minus nil)
           (number (loop
                     (setq byte (%read-byte))
                     (cond ((<= 48 byte 57)
                            (return (- byte 48)))
                           ((zerop byte) ; #\Nul
                            (error "Read EOF or #\Nul."))
                           ((= byte #.(char-code #\-))
                            (setq minus t)))))
           (divisor 1))
      (declare ((integer 0 #.most-positive-fixnum) number))
      (loop
        (setq byte (%read-byte))
        (if (<= 48 byte 57)
            (setq number (+ (- byte 48) (* 10 (the (integer 0 #.(floor most-positive-fixnum 10)) number))))
            (return)))
      (when (= byte #.(char-code #\.))
        (loop
          (setq byte (%read-byte))
          (if (<= 48 byte 57)
              (setq number (+ (- byte 48) (* 10 (the (integer 0 #.(floor most-positive-fixnum 10)) number)))
                    divisor (* 10 (the (integer 0 #.(floor most-positive-fixnum 10)) divisor)))
              (return))))
      (let ((num (coerce (/ number divisor) *read-default-float-format*)))
        (if minus (- num) num)))))
