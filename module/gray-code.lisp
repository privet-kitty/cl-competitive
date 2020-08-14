(defpackage :cp/gray-code
  (:use :cl)
  (:export #:natural-to-gray #:gray-to-natural))
(in-package :cp/gray-code)

(declaim (inline natural-to-gray))
(defun natural-to-gray (x)
  "Encodes X into Gray code."
  (declare (unsigned-byte x))
  (logxor x (ash x -1)))

(declaim (inline natural-to-gray))
(defun gray-to-natural (x)
  "Decodes X from Gray code."
  (declare (unsigned-byte x))
  (do ((pos (- (integer-length x) 2) (- pos 1)))
      ((< pos 0) x)
    (setf (ldb (byte 1 pos) x)
          (logxor (ldb (byte 1 pos) x)
                  (ldb (byte 1 (+ pos 1)) x)))))
