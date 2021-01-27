(defpackage :cp/warshall-floyd
  (:use :cl)
  (:export #:warshall-floyd! #:warshall-floyd-with-restore!))
(in-package :cp/warshall-floyd)

(declaim (inline warshall-floyd!))
(defun warshall-floyd! (matrix)
  "Applies Warshall-Floyd algorithm to MATRIX."
  (let ((n (array-dimension matrix 0)))
    (dotimes (k n)
      (let ((base-k (array-row-major-index matrix k 0)))
        (dotimes (i n)
          (let ((base-i (array-row-major-index matrix i 0)))
            (dotimes (j n)
              (setf (row-major-aref matrix (+ base-i j))
                    (min (row-major-aref matrix (+ base-i j))
                         (+ (row-major-aref matrix (+ base-i k))
                            (row-major-aref matrix (+ base-k j)))))))))))
  matrix)

;; not tested
(declaim (inline warshall-floyd-with-restore!))
(defun warshall-floyd-with-restore! (matrix)
  "Applies Warshall-Floyd algorithm to MATRIX and returns it. In addition this
function returns a matrix NEXTS as the second value: NEXTS[u][v] := a vertex
next to u on the shortest path from vertex u to vertex v.

Example of restoration:
\(defun restore (nexts start goal)
  (loop for v = (aref nexts start goal) then (aref nexts v goal)
        collect v
        until (= v start)))"
  (let* ((n (array-dimension matrix 0))
         (nexts (make-array (list n n) :element-type '(integer 0 #.most-positive-fixnum))))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref nexts i j) j)))
    (dotimes (k n)
      (let ((base-k (array-row-major-index matrix k 0)))
        (dotimes (i n)
          (let ((base-i (array-row-major-index matrix i 0)))
            (dotimes (j n)
              (let ((sum (+ (row-major-aref matrix (+ base-i k))
                            (row-major-aref matrix (+ base-k j)))))
                (when (> (row-major-aref matrix (+ base-i j)) sum)
                  (setf (row-major-aref matrix (+ base-i j)) sum)
                  (setf (row-major-aref nexts (+ base-i j))
                        (row-major-aref nexts (+ base-i k))))))))))
    (values matrix nexts)))
