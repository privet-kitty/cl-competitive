(defpackage :cp/lcp
  (:use :cl :cp/suffix-array :cp/disjoint-sparse-table)
  (:export #:make-lcp-table #:lcp-get))
(in-package :cp/lcp)

(defstruct (lcp-table (:constructor %make-lcp-table (ranks table))
                      (:predicate nil)
                      (:copier nil))
  (ranks nil :type (simple-array sa-int (*)))
  (table nil :type (simple-array sa-int (* *))))

(defun make-lcp-table (sa lcpa)
  "Makes LCP table from a suffix array and an LCP array."
  (declare (optimize (speed 3))
           ((simple-array sa-int (*)) sa lcpa))
  (let* ((n (length sa))
         (ranks (let ((tmp (make-array n :element-type 'sa-int)))
                  (dotimes (i n tmp)
                    (setf (aref tmp (aref sa i)) i))))
         (table (make-disjoint-sparse-table lcpa
                                            (lambda (i j)
                                              (declare (sa-int i j))
                                              (min i j)))))
    (%make-lcp-table ranks table)))

(declaim (ftype (function * (values sa-int &optional)) lcp-get))
(defun lcp-get (lcp-table i j)
  "Returns the length of the longest common prefix of two suffixes starting at i
and j."
  (declare (optimize (speed 3)))
  (let* ((ranks (lcp-table-ranks lcp-table))
         (rank-i (aref ranks i))
         (rank-j (aref ranks j)))
    (if (= rank-i rank-j)
        (- (length ranks) i)
        (dst-fold (lcp-table-table lcp-table)
                  (lambda (x y)
                    (declare (sa-int x y))
                    (min x y))
                  (min rank-i rank-j) (max rank-i rank-j) 0))))
