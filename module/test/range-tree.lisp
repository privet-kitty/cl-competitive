(defpackage :cp/test/range-tree
  (:use :cl :fiveam :cp/range-tree :cp/2dcumul)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/range-tree)
(in-suite base-suite)

;;7...1..
;;6...2..
;;5....3.
;;4.2.4..
;;3......
;;2.....0
;;1.5..7.
;;0......
;; 012345
(test range-tree/manual
  ;; empty case
  (is (null (make-range-tree #())))
  ;; small manual case
  (let* ((points #((1 . 1) (1 . 4) (3 . 4) (3 . 6) (3 . 7) (4 . 1) (4 . 4) (5 . 2)))
         (values #(5 2 4 2 1 7 3 0))
         (rt (make-range-tree #(0 1 2 3 4 5 6 7)
                              :xkey (lambda (i) (car (aref points i)))
                              :ykey (lambda (i) (cdr (aref points i)))
                              :value-key (lambda (i) (aref values i))))
         ;; table of range sum
         (cumuls (make-array '(9 9) :element-type 'fixnum :initial-element 0))
         ;; table of range count
         (counts (make-array '(9 9) :element-type 'fixnum :initial-element 0))
         (state (sb-ext:seed-random-state 0)))
    ;; build CUMULS and COUNTS
    (loop for (x . y) across points
          for value across values
          do (incf (aref cumuls (+ 1 x) (+ 1 y)) value)
             (incf (aref counts (+ 1 x) (+ 1 y)) 1))
    (dotimes (i 9)
      (dotimes (j 8)
        (incf (aref cumuls i (+ j 1)) (aref cumuls i j))
        (incf (aref counts i (+ j 1)) (aref counts i j))))
    (dotimes (j 9)
      (dotimes (i 8)
        (incf (aref cumuls (+ i 1) j) (aref cumuls i j))
        (incf (aref counts (+ i 1) j) (aref counts i j))))

    ;; rt-count
    (is (= 8 (rt-count rt nil nil nil nil)))
    (is (= 8 (rt-count rt 1 1 nil nil)))
    (is (= 8 (rt-count rt 1 1 6 8)))
    (is (= 6 (rt-count rt 1 1 5 7)))
    (is (= 1 (rt-count rt 1 4 3 7)))
    (is (= 4 (rt-count rt 1 4 4 8)))
    (is (= 3 (rt-count rt 1 4 4 7)))
    (is (= 2 (rt-count rt 1 4 4 6)))
    (is (= 2 (rt-count rt 1 4 4 5)))
    (is (= 0 (rt-count rt 1 1 nil 1)))
    (is (= 2 (rt-count rt 4 1 5 5)))
    (is (= 1 (rt-count rt 3 4 4 5)))

    ;; rt-query
    (is (= (reduce #'+ values) (rt-query rt nil nil nil nil)))
    (is (= (reduce #'+ values) (rt-query rt 1 1 nil nil)))
    (is (= 0 (rt-query rt 1 1 nil 1)))
    
    ;; random test
    (finishes
      (dotimes (_ 100)
        (let ((x1 (random 9 state))
              (y1 (random 9 state))
              (x2 (random 9 state))
              (y2 (random 9 state)))
          (when (> x1 x2) (rotatef x1 x2))
          (when (> y1 y2) (rotatef y1 y2))
          (assert (= (rt-count rt x1 y1 x2 y2)
                     (2dcumul-get counts x1 y1 x2 y2)))
          (assert (= (rt-query rt x1 y1 x2 y2)
                     (2dcumul-get cumuls x1 y1 x2 y2))))))))

(defun random-int (inf sup)
  (+ inf (random (- sup inf))))

(defun make-random-case (size inf sup)
  (let ((table (make-hash-table :test #'equal))
        (points (make-array size :element-type 'list)))
    (dotimes (i size)
      (let ((x (random-int inf sup))
            (y (random-int inf sup))
            (value (random-int inf sup)))
        (loop
          (unless (gethash (cons x y) table)
            (return))
          (setq x (random-int inf sup) y (random-int inf sup)))
        (setf (gethash (cons x y) table) value
              (aref points i) (list x y value))))
    (setq points (sort points
                       (lambda (node1 node2)
                         (or (< (first node1) (first node2))
                             (and (= (first node1) (first node2))
                                  (< (second node1) (second node2)))))))
    (values (make-range-tree points :xkey #'first :ykey #'second :value-key #'third)
            table)))

(test range-tree/random
  (finishes
    (dotimes (size 100)
      (multiple-value-bind (rtree table) (make-random-case size -10 10)
        (dotimes (i 100)
          (let ((x1 (if (zerop (random 2)) nil (random-int -12 12)))
                (y1 (if (zerop (random 2)) nil (random-int -12 12)))
                (x2 (if (zerop (random 2)) nil (random-int -12 12)))
                (y2 (if (zerop (random 2)) nil (random-int -12 12))))
            (when (and x1 x2 (> x1 x2))
              (rotatef x1 x2))
            (when (and y1 y2 (> y1 y2))
              (rotatef y1 y2))
            (assert (= (rt-count rtree x1 y1 x2 y2)
                       (loop for (x . y) being each hash-key of table
                             count (and (or (null x1) (<= x1 x))
                                        (or (null x2) (< x x2))
                                        (or (null y1) (<= y1 y))
                                        (or (null y2) (< y y2))))))
            (assert (= (rt-query rtree x1 y1 x2 y2)
                       (loop for (x . y) being each hash-key of table
                             when (and (or (null x1) (<= x1 x))
                                       (or (null x2) (< x x2))
                                       (or (null y1) (<= y1 y))
                                       (or (null y2) (< y y2)))
                             sum (gethash (cons x y) table))))))))))
