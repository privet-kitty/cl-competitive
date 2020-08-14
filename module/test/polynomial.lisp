(defpackage :cp/test/polynomial
  (:use :cl :fiveam :cp/polynomial)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/polynomial)
(in-suite base-suite)

(test poly-floor!
  (declare (notinline poly-floor!))
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1) 10007)
    (is (equalp quot #(9996 3)))
    (is (equalp rem #(9982 41 0 0))))
  (let ((res #(0 0 0 0 0)))
    (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1) 10007 res)
      (is (equalp quot #(9996 3 0 0 0)))
      (is (eq quot res))
      (is (equalp rem #(9982 41 0 0)))))
  ;; leading zeros in dividend
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(-3 2 1) 10007)
    (is (equalp quot #(9996 3)))
    (is (equalp rem #(9982 41 0 0 0 0))))
  ;; leading zeros in divisor
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1 0) 10007)
    (is (equalp quot #(9996 3)))
    (is (equalp rem #(9982 41 0 0))))
  ;; leading zeros in both arguments
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(-3 2 1 0) 10007)
    (is (equalp quot #(9996 3)))
    (is (equalp rem #(9982 41 0 0 0 0))))
  ;; degenerative case
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(10 -5 3 0) 10007)
    (is (equalp quot #(0 1)))
    (is (equalp rem #(8 0 0 0 0 0))))
  ;; division by zero
  (signals division-by-zero (poly-floor! #(8 10 -5 3) #(0 0 0 0) 10007))
  (signals division-by-zero (poly-floor! #(8 10 -5 3) #() 10007))
  (signals division-by-zero (poly-floor! #() #() 10007))
  ;; null dividend
  (multiple-value-bind (quot rem) (poly-floor! #() #(10 -5 3 0) 10007)
    (is (equalp quot #()))
    (is (equalp rem #()))))

(test poly-mod!
  (declare (notinline poly-mod!))
  (is (equalp #(9982 41 0 0)
              (poly-mod! #(8 10 -5 3) #(-3 2 1) 10007)))
  ;; (EQ <dividend> <result>)
  (let ((dividend #(8 10 -5 3)))
    (is (eq dividend (poly-mod! dividend #(-3 2 1) 10007))))
  ;; leading zeros in dividend
  (is (equalp #(9982 41 0 0 0 0)
              (poly-mod! #(8 10 -5 3 0 0) #(-3 2 1) 10007)))
  ;; leading zeros in divisor
  (is (equalp #(9982 41 0 0)
              (poly-mod! #(8 10 -5 3) #(-3 2 1 0) 10007)))
  ;; leading zeros in both arguments
  (is (equalp #(9982 41 0 0 0 0)
              (poly-mod! #(8 10 -5 3 0 0) #(-3 2 1 0) 10007)))
  ;; degenerative case
  (is (equalp #(8 0 0 0 0 0)
              (poly-mod! #(8 10 -5 3 0 0) #(10 -5 3 0) 10007)))
  ;; division by zero
  (signals division-by-zero (poly-mod! #(8 10 -5 3) #(0 0 0 0) 10007))
  (signals division-by-zero (poly-mod! #(8 10 -5 3) #() 10007))
  (signals division-by-zero (poly-mod! #() #() 10007))
  ;; null dividend
  (is (equalp #() (poly-mod! #() #(10 -5 3 0) 10007))))
