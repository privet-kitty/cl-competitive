;;;
;;; Bron-Kerbosch-Tomita
;;; Reference: http://www.dcs.gla.ac.uk/~pat/jchoco/clique/enumeration/report.pdf
;;;

(defpackage :cp/bron-kerbosch
  (:use :cl)
  (:export #:find-max-clique))
(in-package :cp/bron-kerbosch)

;; TODO: take an ordinary adjacency matrix as an input
(declaim (inline find-max-clique))
(defun find-max-clique (neighbors)
  "Returns an unsigned fixnum as the bit-set that indicates a maximum
clique. Each NEIGHBORS[i] must also be the unsigned fixnum that indicates the
neighbors of vertex i."
  (let ((n (length neighbors))
        (result-set 0)
        (result-size 0))
    (declare ((integer 0 #.most-positive-fixnum) result-set result-size))
    (labels ((tzcount (x)
               (max 0 (- (integer-length (logand x (- x))) 1)))
             (recur (r p x)
               (declare ((integer 0 #.most-positive-fixnum) r p x))
               (if (zerop p)
                   (when (zerop x)
                     (let ((size (logcount r)))
                       (when (> size result-size)
                         (setq result-set r
                               result-size size))))
                   (let ((pivot 0)
                         (max -1)
                         (p-or-x (logior p x)))
                     ;; Find a vertex in P∪X that has the most neighbors in P
                     ;; and use it as a pivot vertex
                     (loop for u from (tzcount p-or-x) below (integer-length p-or-x)
                           do (when (logbitp u p-or-x)
                                (let ((num-neighbors (logcount (logand p (aref neighbors u)))))
                                  (when (> num-neighbors max)
                                    (setq pivot u
                                          max num-neighbors)))))
                     (let ((pivot-neighbors (logandc2 p (aref neighbors pivot))))
                       (unless (zerop pivot-neighbors)
                         (loop for v from (tzcount pivot-neighbors) below (integer-length pivot-neighbors)
                               do (when (logbitp v p)
                                    (recur (dpb 1 (byte 1 v) r)
                                           (logand p (aref neighbors v))
                                           (logand x (aref neighbors v)))
                                    (setf (ldb (byte 1 v) p) 0
                                          (ldb (byte 1 v) x) 1)))))))))
      (declare (inline tzcount))
      (recur 0 (- (ash 1 n) 1) 0)
      result-set)))
