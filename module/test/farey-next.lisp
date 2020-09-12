(defpackage :cp/test/farey-next
  (:use :cl :fiveam :cp/farey-next :cp/farey)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/farey-next)
(in-suite base-suite)

(defun make-farey-seq (max-denom)
  (let (res)
    (push (cons 0 1) res)
    (map-farey (lambda (num denom) (push (cons num denom) res))
               max-denom)
    (push (cons 1 1) res)
    (nreverse res)))

(test farey-next
  (loop for level from 1 to 10
        for list = (make-farey-seq level)
        do (is (loop for (ratio1 ratio2) on list
                     while ratio2
                     always (and (multiple-value-bind (num-next denom-next)
                                     (farey-next (car ratio1) (cdr ratio1) level)
                                   (equal (cons num-next denom-next) ratio2))
                                 (multiple-value-bind (num-prev denom-prev)
                                     (farey-prev (car ratio2) (cdr ratio2) level)
                                   (equal (cons num-prev denom-prev) ratio1)))))))
