(defpackage :cp/zeta-transform
  (:use :cl)
  (:export #:zeta-subtransform! #:zeta-supertransform!
           #:moebius-subtransform! #:moebius-supertransform!)
  (:documentation "Provides Zeta/Moebius transforms w.r.t. subsets and
supersets."))
(in-package :cp/zeta-transform)

;; TODO: Should we integrate zeta- and moebius- into a function?

(declaim (inline zeta-subtransform!))
(defun zeta-subtransform! (vector &optional (plus #'+))
  "Does the fast zeta transform w.r.t. subsets. The length of VECTOR must be a
power of two."
  (declare (vector vector))
  (let* ((n (length vector))
         ;; cardinality of the underlying set
         (card (- (integer-length n) 1)))
    (assert (= 1 (logcount n)))
    (dotimes (i card)
      (let ((mask (ash 1 i)))
        (dotimes (j n)
          (unless (zerop (logand j mask))
            (setf (aref vector j)
                  (funcall plus
                           (aref vector j)
                           (aref vector (logxor j mask))))))))
    vector))

(declaim (inline zeta-supertransform!))
(defun zeta-supertransform! (vector &optional (plus #'+))
  "Does the fast zeta transform w.r.t. supersets. The length of VECTOR must be a
power of two."
  (declare (vector vector))
  (let* ((n (length vector))
         (card (- (integer-length n) 1)))
    (assert (= 1 (logcount n)))
    (dotimes (i card)
      (let ((mask (ash 1 i)))
        (dotimes (j n)
          (when (zerop (logand j mask))
            (setf (aref vector j)
                  (funcall plus
                           (aref vector j)
                           (aref vector (logior j mask))))))))
    vector))

(declaim (inline moebius-subtransform!))
(defun moebius-subtransform! (vector &optional (minus #'-))
  "Does the inverse of ZETA-SUBTRANSFORM! The length of VECTOR must be a power
of two."
  (declare (vector vector))
  (let* ((n (length vector))
         (card (- (integer-length n) 1)))
    (assert (= 1 (logcount n)))
    (dotimes (i card)
      (let ((mask (ash 1 i)))
        (dotimes (j n)
          (unless (zerop (logand j mask))
            (setf (aref vector j)
                  (funcall minus
                           (aref vector j)
                           (aref vector (logxor j mask))))))))
    vector))

(declaim (inline moebius-supertransform!))
(defun moebius-supertransform! (vector &optional (minus #'-))
  "Does the inverse of ZETA-SUPERTRANSFORM!. The length of VECTOR must be a
power of two."
  (declare (vector vector))
  (let* ((n (length vector))
         (card (- (integer-length n) 1)))
    (assert (= 1 (logcount n)))
    (dotimes (i card)
      (let ((mask (ash 1 i)))
        (dotimes (j n)
          (when (zerop (logand j mask))
            (setf (aref vector j)
                  (funcall minus
                           (aref vector j)
                           (aref vector (logior j mask))))))))
    vector))
