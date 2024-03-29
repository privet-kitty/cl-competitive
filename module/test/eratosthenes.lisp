(defpackage :cp/test/eratosthenes
  (:use :cl :fiveam :cp/eratosthenes)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/eratosthenes)
(in-suite base-suite)

(test make-prime-table
  (is (equalp #* (make-prime-table 0)))
  (is (equalp #*0 (make-prime-table 1)))
  (is (equalp #*00 (make-prime-table 2)))
  (is (equalp #*001 (make-prime-table 3)))
  (is (equalp #*0011010100 (make-prime-table 10)))
  (is (equalp #*001101010001010001010001000001 (make-prime-table 30)))
  (finishes
    (loop for b across (make-prime-table 200)
          for i below 200
          when (= b 1)
          do (assert (sb-int:positive-primep i))))
  ;; word boundary
  (finishes
    (loop for b across (make-prime-table 64)
          for i below 64
          when (= b 1)
          do (assert (sb-int:positive-primep i)))))

(test make-prime-sequence
  (is (equalp #() (make-prime-sequence 0)))
  (is (equalp #() (make-prime-sequence 1)))
  (is (equalp #() (make-prime-sequence 2)))
  (is (equalp #(2) (make-prime-sequence 3)))
  (is (equalp #(2 3 5 7) (make-prime-sequence 10)))
  (is (equalp #(2 3 5 7) (make-prime-sequence 11)))
  (is (equalp #(2 3 5 7 11 13 17 19 23 29) (make-prime-sequence 30))))

(test factorize
  (is (null (factorize 1 (make-prime-data 2))))
  (is (null (factorize 1 (make-prime-data 20))))
  (let ((pdata (make-prime-data 20)))
    (dotimes (_ 10)
      (is (equal '((2 . 2) (3 . 2) (7 . 1))
                 (factorize 252 pdata)))))
  (is (equal '((2 . 2) (3 . 2) (7 . 1))
             (factorize 252 (make-prime-data 7))))
  (is (equal '((2 . 2) (3 . 2) (7 . 1))
             (factorize 252 (make-prime-data 4))))
  (is (equal '((2 . 2) (63 . 1))
             (factorize 252 (make-prime-data 3)))))
