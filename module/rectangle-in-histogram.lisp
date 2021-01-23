(defpackage :cp/rectangle-in-histogram
  (:use :cl)
  (:export #:maximize-rectangle-in-histogram))
(in-package :cp/rectangle-in-histogram)

;; This implementation is intended for later reference. So we try to keep it
;; simple instead of extending it or speeding it up.

(defun maximize-rectangle-in-histogram (heights)
  "Maximize the area of rectangles within given histogram. Returns three values:
maximum area, (inclusive) left index and (exclusive) right index of the maximum
rectangle."
  (declare (vector heights))
  (let (stack ; (height . index)
        (width (length heights))
        (max-area 0)
        (l 0)
        (r 0)) 
    (dotimes (i (+ 1 width))
      (let ((h (if (= i width)
                   most-negative-fixnum ; negative infinity
                   (aref heights i))))
        (loop with new-i = i
              while stack
              for (prev-h . prev-i) = (car stack)
              for area = (* prev-h (- i prev-i))
              while (> prev-h h)
              when (> area max-area)
              do (setq max-area area
                       l prev-i
                       r i)
              do (setq new-i prev-i)
                 (pop stack)
              finally (push (cons h new-i) stack))))
    (values max-area l r)))
