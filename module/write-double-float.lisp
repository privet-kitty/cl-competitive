(defpackage :cp/write-double-float
  (:use :cl)
  (:export #:write-double-float))
(in-package :cp/write-double-float)

;; Based on SBCL's implementation
(defun write-double-float (x &key (max-decimal-places 10)
                                  (stream *standard-output*)
                                  (allow-trailing-point t))
  "Writes a double float X to STREAM using a fixed-point expression.

MAX-DECIMAL-PLACES is the maximum number of digits after decimal point. (The
actual number of output digits can be less than this number, however.)

If ALLOW-TRAILING-POINT is false, an expression like `123.' is modified to
`123.0'"
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) max-decimal-places)
           (double-float x))
  (if (minusp x)
      (progn (write-char #\- stream)
             (when (> x -1d0)
               (write-char #\0 stream)))
      (when (< x 1d0)
        (write-char #\0 stream)))
  (multiple-value-bind (e string)
      (sb-impl::flonum-to-digits x)
    (declare (fixnum e)
             (simple-base-string string))
    (if (plusp e)
        (let* ((len (length string))
               (len-before-. (min len e))
               (len-after-. (- (min (+ len-before-. max-decimal-places) len)
                               len-before-.)))
          (write-string string stream :end len-before-.)
          (dotimes (i (- e len))
            (write-char #\0 stream))
          (write-char #\. stream)
          (write-string string stream :start len-before-.
                                      :end (min (+ len-before-. max-decimal-places) len))
          (when (and (not allow-trailing-point)
                     (zerop len-after-.))
            (write-char #\0 stream)))
        (let* ((len-0 (min (- e) max-decimal-places))
               (len-after-0 (min (length string)
                                 (the fixnum (- max-decimal-places len-0)))))
          (write-char #\. stream)
          (dotimes (i len-0)
            (write-char #\0 stream))
          (write-string string stream :end len-after-0)
          (when (and (not allow-trailing-point)
                     (zerop len-after-0))
            (write-char #\0 stream))))))
