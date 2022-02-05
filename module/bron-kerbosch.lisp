(defpackage :cp/bron-kerbosch
  (:use :cl)
  (:export #:find-max-clique)
  (:documentation "Provides Bron-Kerbosch-Tomita algorithm for maximum clique
problem.

Reference:
http://www.dcs.gla.ac.uk/~pat/jchoco/clique/enumeration/report.pdf"))
(in-package :cp/bron-kerbosch)

;; NOTE: not tested

(defconstant +nbits+ 62)
(deftype uint () '(unsigned-byte #.+nbits+))

(declaim (inline lsb))
(defun lsb (x)
  (logand x (- x)))

(declaim (inline find-max-clique))
(defun find-max-clique (neighbors)
  "Returns an unsigned fixnum as the bit-set that indicates a maximum
clique. Each NEIGHBORS[i] must also be the unsigned fixnum that indicates the
neighbors of vertex i."
  (let ((n (length neighbors))
        (result-set 0)
        (result-size 0))
    (declare ((integer 0 #.+nbits+) n)
             (uint result-set result-size))
    (labels ((recur (r p x)
               (declare (uint r p x))
               (if (zerop p)
                   (when (zerop x)
                     (let ((size (logcount r)))
                       (when (> size result-size)
                         (setq result-set r
                               result-size size))))
                   (let ((pivot 0)
                         (max -1)
                         (p-or-x (logior p x)))
                     (declare (uint p-or-x))
                     ;; Find a vertex in P \cup X that has the most neighbors in P
                     ;; and use it as a pivot vertex
                     (loop (when (zerop p-or-x)
                             (return))
                           (let* ((lsb (lsb p-or-x))
                                  (u (- (integer-length lsb) 1))
                                  (num-neighbors (logcount (logand p (aref neighbors u)))))
                             (when (> num-neighbors max)
                               (setq pivot u
                                     max num-neighbors))
                             (setq p-or-x (logxor p-or-x lsb))))
                     (let ((pivot-neighbors (logandc2 p (aref neighbors pivot))))
                       (declare (uint pivot-neighbors))
                       (loop (when (zerop pivot-neighbors)
                               (return))
                             (let* ((lsb (lsb pivot-neighbors))
                                    (v (- (integer-length lsb) 1)))
                               (recur (dpb 1 (byte 1 v) r)
                                      (logand p (aref neighbors v))
                                      (logand x (aref neighbors v)))
                               (setf (ldb (byte 1 v) p) 0
                                     (ldb (byte 1 v) x) 1)
                               (setq pivot-neighbors (logxor pivot-neighbors lsb)))))))))
      (recur 0 (- (ash 1 n) 1) 0)
      result-set)))
