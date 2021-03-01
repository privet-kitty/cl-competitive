(defpackage :cp/test/segment-tree
  (:use :cl :fiveam :cp/segment-tree)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/segment-tree)
(in-suite base-suite)

(define-segtree segtree
  :operator #'+
  :identity 0
  :element-type (unsigned-byte 31))

(test segtree/hand
  (let ((segtree (make-segtree 0)))
    (is (zerop (segtree-fold segtree 0 0)))))

(test segtree-bisect
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (n 100)
      (let* ((vec (make-array n :element-type '(unsigned-byte 31) :initial-element 0)))
        (dotimes (i n)
          (setf (aref vec i) (random 10)))
        (let ((segtree (make-segtree n :initial-contents vec))
              (total-sum (reduce #'+ vec)))
          (is (= total-sum (segtree-fold segtree 0 n)))
          (loop for pivot from n downto 0
                do (dotimes (_ 20)
                     ;; min-left
                     (let* ((threshold (random (+ total-sum 10)))
                            (l1 (segtree-min-left segtree
                                                  (lambda (x) (<= x threshold))
                                                  pivot))
                            (l2 pivot)
                            (sum 0))
                       (loop while (and (> l2 0)
                                        (<= (+ sum (aref vec (- l2 1))) threshold))
                             do (decf l2)
                                (incf sum (aref vec l2)))
                       (is (= l1 l2)))
                     ;; max-right
                     (let* ((threshold (random (+ total-sum 10)))
                            (r1 (segtree-max-right segtree
                                                   (lambda (x) (<= x threshold))
                                                   pivot))
                            (r2 pivot)
                            (sum 0))
                       (loop while (and (< r2 n)
                                        (<= (+ sum (aref vec r2)) threshold))
                             do (incf sum (aref vec r2))
                                (incf r2))
                       (is (= r1 r2))))))))))
