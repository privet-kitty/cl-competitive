(defpackage :cp/merge-sort
  (:use :cl)
  (:export #:merge-sort!)
  (:documentation "Provides merge sort with bottom-up implementation."))
(in-package :cp/merge-sort)

(declaim (inline %merge))
(defun %merge (l mid r source-vec dest-vec order key)
  (declare ((mod #.array-dimension-limit) l mid r)
           (function order key))
  (loop with pos1 = l
        with pos2 = mid
        for dest from l
        when (= pos1 mid)
        do (loop for pos2 from pos2 below r
                 for dest from dest
                 do (setf (aref dest-vec dest)
                          (aref source-vec pos2))
                 finally (return-from %merge))
        when (= pos2 r)
        do (loop for pos1 from pos1 below mid
                 for dest from dest
                 do (setf (aref dest-vec dest)
                          (aref source-vec pos1))
                 finally (return-from %merge))
        do (if (funcall order
                        (funcall key (aref source-vec pos1))
                        (funcall key (aref source-vec pos2)))
               (setf (aref dest-vec dest) (aref source-vec pos1)
                     pos1 (1+ pos1))
               (setf (aref dest-vec dest) (aref source-vec pos2)
                     pos2 (1+ pos2)))))

;; NOTE: This merge sort is slow on SBCL version earlier than 1.5.0 as the type
;; propagation of MAKE-ARRAY doesn't work.

;; TODO: Peephole optimization of SBCL is not sufficient to optimize identity
;; KEY function. Defining deftransform will work.
(declaim (inline merge-sort!))
(defun merge-sort! (vector order &key (start 0) end (key #'identity))
  "Destructively sorts VECTOR. You can rely on the side effect. Note that this
sort is not stable.

ORDER := strict order."
  (declare (vector vector)
           (function order key))
  (let* ((end (or end (length vector)))
         ;; TODO: avoid to allocate excessive size
         (buffer (make-array end :element-type (array-element-type vector))))
    (declare ((mod #.array-dimension-limit) start end))
    (loop for width of-type (mod #.array-dimension-limit) = 1 then (* width 2)
          for vec1 = vector then vec2
          and vec2 = buffer then vec1
          while (< width end)
          do (loop for l from start below end by (* width 2)
                   for mid = (min end (+ l width))
                   for r = (min end (+ mid width))
                   do (%merge l mid r vec1 vec2 order key))
          finally (unless (eq vec1 vector)
                    (replace vector buffer
                             :start1 start :end1 end
                             :start2 start :end2 end)))
    vector))
