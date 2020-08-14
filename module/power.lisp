;;;
;;; Calculate a^n on any monoids in O(log(n)) time
;;;

(defpackage :cp/power
  (:use :cl)
  (:export #:power))
(in-package :cp/power)

;; TODO: test
;; TODO: non-recursive implementation
(declaim (inline power))
(defun power (base exponent op identity)
  "OP := binary operation (comprising a monoid)
IDENTITY := identity element w.r.t. OP"
  (declare ((integer 0) exponent))
  (labels ((recur (x p)
             (declare ((integer 0 #.most-positive-fixnum) p))
             (cond ((zerop p) identity)
                   ((evenp p) (recur (funcall op x x) (ash p -1)))
                   (t (nth-value 0 (funcall op x (recur x (- p 1)))))))
           (recur-big (x p)
             (declare ((integer 0) p))
             (cond ((zerop p) identity)
                   ((evenp p) (recur-big (funcall op x x) (ash p -1)))
                   (t (nth-value 0 (funcall op x (recur-big x (- p 1))))))))
    (typecase exponent
      (fixnum (recur base exponent))
      (otherwise (recur-big base exponent)))))
