(defpackage :cp/test/convex-hull-trick
  (:use :cl :fiveam :cp/convex-hull-trick :cp/find-argopt)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/convex-hull-trick)
(in-suite base-suite)

(defun %random ()
  (- (random 100) 50))

(test convex-hull-trick/handmade
  ;; Examples by satanic0258
  ;; https://satanic0258.hatenablog.com/entry/2016/08/16/181331
  (let ((cht (make-cht 10)))
    (cht-push cht 2 2)
    (cht-push cht 1 3)
    (cht-push cht -1 10)
    (cht-push cht -2 15)
    (is (= 0 (cht-get cht -1)))
    (is (= 2 (cht-get cht 0)))
    (is (= 4 (cht-get cht 1)))
    (is (= 5 (cht-get cht 2)))
    (is (= 6 (cht-get cht 3)))
    (is (= 6 (cht-get cht 4)))
    (is (= 1 (cht-get cht 7))))

  (let ((cht (make-cht 10)))
    (cht-push cht 2 0)
    (is (= -4 (cht-get cht -2)))
    (is (= 4 (cht-get cht 2)))
    (cht-push cht 1 1)
    (cht-push cht 0 -1)
    (is (= -4 (cht-get cht -2)))
    (is (= -1 (cht-get cht 2)))
    (cht-push cht -1 0)
    (is (= 3 (cp/convex-hull-trick::%cht-length cht)))
    (is (= -4 (cht-get cht -2)))
    (is (= -2 (cht-get cht 2)))))

(defun %make-sample (size order &optional (minimum t))
  (let ((slopes (sort (loop repeat size collect (%random)) order))
        (intercepts (loop repeat size collect (%random)))
        (cht (make-cht size minimum)))
    (loop for slope in slopes
          for intercept in intercepts
          do (cht-push cht slope intercept))
    (values cht slopes intercepts)))

(defun %optimize (x slopes intercepts &optional (minimum t))
  (if minimum
      (loop for slope in slopes
            for intercept in intercepts
            minimize (+ (* x slope) intercept))
      (loop for slope in slopes
            for intercept in intercepts
            maximize (+ (* x slope) intercept))))

(test convex-hull-trick/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dolist (minimum (list t nil))
      ;; monotone increasing slopes
      (dotimes (_ 20)
        (multiple-value-bind (cht slopes intercepts)
            (%make-sample 100 #'< minimum)
          (loop for x from -50 to 50
                do (let ((min1 (cht-get cht x))
                         (min2 (%optimize x slopes intercepts minimum)))
                     (is (= min1 min2))))))
      ;; monotone decreasing slopes
      (dotimes (_ 20)
        (multiple-value-bind (cht slopes intercepts)
            (%make-sample 100 #'> minimum)
          (loop for x from -50 to 50
                do (let ((min1 (cht-get cht x))
                         (min2 (%optimize x slopes intercepts minimum)))
                     (is (= min1 min2))))))
      ;; monotone increasing query
      (dotimes (_ 20)
        (multiple-value-bind (cht slopes intercepts)
            (%make-sample 100 #'< minimum)
          (loop for x from -50 to 50
                do (let ((min1 (cht-increasing-get cht x))
                         (min2 (%optimize x slopes intercepts minimum)))
                     (is (= min1 min2))))))
      ;; monotone decreasing query
      (dotimes (_ 20)
        (multiple-value-bind (cht slopes intercepts)
            (%make-sample 100 #'> minimum)
          (loop for x from 50 downto -50
                do (let ((min1 (cht-decreasing-get cht x))
                         (min2 (%optimize x slopes intercepts minimum)))
                     (is (= min1 min2)))))))))
