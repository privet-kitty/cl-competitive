;;;
;;; Diameter of tree
;;;

(defpackage :cp/diameter
  (:use :cl)
  (:export #:find-diameter))
(in-package :cp/diameter)

(declaim (inline find-diameter))
(defun find-diameter (graph)
  "Finds a diameter of a tree. Returns three values: the length of the diameter
and its two ends."
  (declare (vector graph))
  (let ((end 0)
        (max-depth 0))
    (declare ((mod #.array-total-size-limit) end max-depth))
    (assert (> (length graph) 0))
    (labels ((traverse (v parent depth)
               (declare ((integer 0 #.array-total-size-limit) v parent depth))
               (when (> depth max-depth)
                 (setq max-depth depth
                       end v))
               (dolist (child (aref graph v))
                 (declare ((mod #.array-total-size-limit) child))
                 (unless (= child parent)
                   (traverse child v (+ depth 1))))))
      (traverse 0 array-total-size-limit 0)
      (let ((end1 end))
        (setq max-depth 0)
        (traverse end1 array-total-size-limit 0)
        (values max-depth end1 end)))))
