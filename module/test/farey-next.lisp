(defpackage :cp/test/farey-next
  (:use :cl :fiveam :cp/farey-next :cp/farey :cp/phase)
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

(test farey-next/prev
  (is (equal (multiple-value-list (farey-next 1 2 8))
             (multiple-value-list (farey-next 2 4 8))))
  (is (equal (multiple-value-list (farey-prev 1 2 8))
             (multiple-value-list (farey-prev 2 4 8))))
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

(defun make-grids-seq (max-denom)
  (let (res)
    (loop for x from (- max-denom) to max-denom
          do (loop for y from (- max-denom) to max-denom
                   unless (or (= x y 0)
                              (> (gcd x y) 1))
                   do (push (complex x y) res)))
    (setq res (sort res #'phase<))
    (append res (list (car res)))))

(test phase-next/prev
  (loop for level from 1 to 10
        for list = (make-grids-seq level)
        do (is (loop for (p1 p2) on list
                     while p2
                     always (= p2 (phase-next p1 level))))
           (is (loop for (p1 p2) on list
                     while p2
                     always (= p1 (phase-prev p2 level))))))
