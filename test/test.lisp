(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util.lisp")
  (load "../ext-gcd.lisp")
  (load "../bounded-partition-number.lisp")
  (load "../bisect.lisp")
  (load "../bipartite-matching.lisp"))

(use-package :test-util)
(setf *elapsed-times* nil)

;;;
;;; ext-gcd.lisp
;;;

(with-test (:name mod-log)
  (dotimes (i 100)
    (let ((a (- (random 20) 10))
          (b (- (random 20) 10)))
      (multiple-value-bind (x y) (ext-gcd a b)
        (assert (= (+ (* a x) (* b y)) (gcd a b))))))
  (assert (= 8 (mod-log 6 4 44)))
  (assert (= 8 (mod-log -38 -40 44)))
  (assert (null (mod-log 6 2 44)))
  (assert (= 2 (mod-log 8 4 12)))
  (assert (= 4 (mod-log 3 13 17)))
  (assert (= 1 (mod-log 12 0 4)))
  (assert (= 2 (mod-log 12 0 8)))
  (assert (null (mod-log 12 1 8)))
  (assert (= 1 (mod-log 0 0 100))))

(with-test (:name mod-inverse)
  (dotimes (i 1000)
    (let ((a (random 100))
          (m (+ 2 (random 100))))
      (assert (or (/= 1 (gcd a m))
                  (= 1 (mod (* a (mod-inverse a m)) m)))))))

(with-test (:name solve-bezout)
  (assert (= (calc-min-factor 8 3) -2))
  (assert (= (calc-min-factor -8 3) 3))
  (assert (= (calc-min-factor 8 -3) 2))
  (assert (= (calc-min-factor -8 -3) -3))
  (assert (= (calc-max-factor 8 3) -3))
  (assert (= (calc-max-factor -8 3) 2))
  (assert (= (calc-max-factor 8 -3) 3))
  (assert (= (calc-max-factor -8 -3) -2)))

;;;
;;; bounded-partition-number.lisp
;;;

(with-test (:name make-bpartition)
  (let ((table (make-bpartition 1 0 100000)))
    (assert (= 1 (aref table 0 0)))
    (assert (equal '(1 1) (array-dimensions table))))
  (let ((table (make-bpartition 361 25 100000)))
    (assert (= 74501 (aref table 360 25)))
    (assert (= (aref table 3 3) (aref table 3 4) (aref table 3 5)))
    (assert (= 0 (aref table 3 0)))
    (assert (= 1 (aref table 0 0)))
    (assert (= 1 (aref table 0 1)))))

;;;
;;; bisect.lisp
;;;

(with-test (:name bisect-left)
  (assert (= 0 (bisect-left #(1 8) -3)))
  (assert (= 0 (bisect-left #(1 8) 1)))
  (assert (= 1 (bisect-left #(1 8) 4)))
  (assert (= 1 (bisect-left #(1 8) 8)))
  (assert (= 2 (bisect-left #(1 8) 9)))
  (assert (= 3 (bisect-left #(1 4 5 7 7 7 7 7 7 8) 7)))
  (assert (= 3 (bisect-left #(1 4 4 7 7 7 7 7 8) 6)))
  (assert (= 1 (bisect-left #(#\a #\c #\c #\d) #\b :test #'char<)))
  (assert (= 4 (bisect-left #(nil 1 4 4 7 7 nil nil) 6 :start 1 :end 4))))

(with-test (:name bisect-right)
  (assert (= 0 (bisect-right #(1) 0)))
  (assert (= 1 (bisect-right #(1) 1)))
  (assert (= 1 (bisect-right #(1) 2)))
  (assert (= 0 (bisect-right #(1 8) 0)))
  (assert (= 2 (bisect-right #(1 8) 8)))
  (assert (= 1 (bisect-right #(1 8) 4)))
  (assert (= 1 (bisect-right #(1 8) 1)))
  (assert (= 2 (bisect-right #(1 8) 9)))
  (assert (= 7 (bisect-right #(1 4 5 7 7 7 7 8) 7)))
  (assert (= 3 (bisect-right #(1 4 4 7 7 7 7 7 8) 6)))
  (assert (= 3 (bisect-right #(10 9 9 7 7 7 7 7 4) 9 :test #'>)))
  (assert (= 3 (bisect-right #(#\a #\c #\c #\d) #\c :test #'char<)))
  (assert (= 4 (bisect-right #(nil 1 4 4 4 4 7 7 nil nil) 4 :start 1 :end 4))))

;;;
;;; bipartite-matching.lisp
;;;


(with-test (:name find-matcning)
  (let* ((graph (make-array 9
                            :element-type 'list
                            :initial-contents '((6) (5 6 7 8) (6) (6) (5) (1 4) (0 1 2 3) (1) (1))))
         (matching (find-matching graph)))
    (loop for i below 9
          do (assert (or (= (aref matching i) -1)
                         (= i (aref matching (aref matching i))))))
    (assert (= 6 (count -1 matching :test-not #'=)))))
