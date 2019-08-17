(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../range-tree.lisp")
  (load "../2dcumul.lisp"))

(use-package :test-util)

(declaim (notinline make-range-tree))

;;7...1..
;;6...2..
;;5....3.
;;4.2.4..
;;3......
;;2.....0
;;1.5..7.
;;0......
;; 012345
(with-test (:name range-tree)
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
    (assert (= 8 (rt-count rt nil nil nil nil)))
    (assert (= 8 (rt-count rt 1 1 nil nil)))
    (assert (= 8 (rt-count rt 1 1 6 8)))
    (assert (= 6 (rt-count rt 1 1 5 7)))
    (assert (= 1 (rt-count rt 1 4 3 7)))
    (assert (= 4 (rt-count rt 1 4 4 8)))
    (assert (= 3 (rt-count rt 1 4 4 7)))
    (assert (= 2 (rt-count rt 1 4 4 6)))
    (assert (= 2 (rt-count rt 1 4 4 5)))
    (assert (= 0 (rt-count rt 1 1 nil 1)))
    (assert (= 2 (rt-count rt 4 1 5 5)))
    (assert (= 1 (rt-count rt 3 4 4 5)))

    ;; rt-query
    (assert (= (reduce #'+ values) (rt-query rt nil nil nil nil)))
    (assert (= (reduce #'+ values) (rt-query rt 1 1 nil nil)))
    (assert (= 0 (rt-query rt 1 1 nil 1)))
    
    ;; random test
    (dotimes (_ 100)
      (let ((x1 (random 9 state))
            (y1 (random 9 state))
            (x2 (random 9 state))
            (y2 (random 9 state)))
        (when (> x1 x2) (rotatef x1 x2))
        (when (> y1 y2) (rotatef y1 y2))
        (assert (= (rt-count rt x1 y1 x2 y2)
                   (get-2dcumul counts x1 y1 x2 y2)))
        (assert (= (rt-query rt x1 y1 x2 y2)
                   (get-2dcumul cumuls x1 y1 x2 y2)))))))
