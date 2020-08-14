(defpackage :cp/test/octet-input-stream
  (:use :cl)
  (:export #:octet-input-stream #:make-octet-input-stream))
(in-package :cp/test/octet-input-stream)

;; acknowledge: https://stackoverflow.com/questions/41378669/how-to-get-a-stream-from-a-bit-vector-in-common-lisp
(defclass octet-input-stream (sb-gray:fundamental-binary-input-stream)
  ((data :initarg :data :type (vector (unsigned-byte 8)))
   (position :initform 0)))

(defmethod stream-element-type ((stream octet-input-stream))
  '(unsigned-byte 8))

(defmethod sb-gray:stream-read-byte ((stream octet-input-stream))
  (with-slots (data position) stream
    (if (< position (length data))
        (prog1 (aref data position)
          (incf position))
        :eof)))

#+swank
(defmethod sb-gray:stream-read-char ((stream octet-input-stream))
  (let ((res (sb-gray:stream-read-byte stream)))
    (if (eql res :eof)
        :eof
        (code-char res))))

(defun make-octet-input-stream (data)
  (etypecase data
    (string (let ((octets (make-array (length data) :element-type '(unsigned-byte 8))))
              (dotimes (i (length data))
                (setf (aref octets i) (char-code (aref data i))))
              (make-instance 'octet-input-stream :data octets)))
    (sequence (make-instance 'octet-input-stream
                              :data (coerce data '(simple-array (unsigned-byte 8) (*)))))))
