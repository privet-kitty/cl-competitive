(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../convex-hull-trick.lisp")
  (load "../find-argopt.lisp"))

(use-package :test-util)

(defun %random ()
  (- (random 100) 100))

(with-test (:name convex-hull-trick/handmade)
  ;; Examples by satanic0258
  ;; https://satanic0258.hatenablog.com/entry/2016/08/16/181331
  (let ((cht (make-cht 10)))
    (cht-push cht 2 2)
    (cht-push cht 1 3)
    (cht-push cht -1 10)
    (cht-push cht -2 15)
    (assert (= 0 (cht-get cht -1)))
    (assert (= 2 (cht-get cht 0)))
    (assert (= 4 (cht-get cht 1)))
    (assert (= 5 (cht-get cht 2)))
    (assert (= 6 (cht-get cht 3)))
    (assert (= 6 (cht-get cht 4)))
    (assert (= 1 (cht-get cht 7))))

  (let ((cht (make-cht 10)))
    (cht-push cht 2 0)
    (assert (= -4 (cht-get cht -2)))
    (assert (= 4 (cht-get cht 2)))
    (cht-push cht 1 1)
    (cht-push cht 0 -1)
    (assert (= -4 (cht-get cht -2)))
    (assert (= -1 (cht-get cht 2)))
    (cht-push cht -1 0)
    (assert (= 3 (%cht-length cht)))
    (assert (= -4 (cht-get cht -2)))
    (assert (= -2 (cht-get cht 2)))))

(defun %make-sample (size order &optional (minimum t))
  (let ((slopes (sort (loop repeat size collect (%random)) order))
        (intercepts (loop repeat size collect (%random)))
        (cht (make-cht size minimum)))
    (loop for slope in slopes
          for intercept in intercepts
          do (cht-push cht slope intercept))
    (values cht slopes intercepts)))

(defun %optimize (cht x slopes intercepts &optional (minimum t))
  (if minimum
      (loop for slope in slopes
            for intercept in intercepts
            minimize (+ (* x slope) intercept))
      (loop for slope in slopes
            for intercept in intercepts
            maximize (+ (* x slope) intercept))))

(with-test (:name convex-hull-trick/random)
  (dolist (minimum '(t nil))
    ;; monotone increasing slopes
    (multiple-value-bind (cht slopes intercepts) (%make-sample 100 #'< minimum)
      (loop for x from -50 to 50
            do (let ((min1 (cht-get cht x))
                     (min2 (%optimize cht x slopes intercepts minimum)))
                 (assert (= min1 min2)))))
    ;; monotone decreasing slopes
    (multiple-value-bind (cht slopes intercepts) (%make-sample 100 #'> minimum)
      (loop for x from -50 to 50
            do (let ((min1 (cht-get cht x))
                     (min2 (%optimize cht x slopes intercepts minimum)))
                 (assert (= min1 min2)))))
    ;; monotone increasing query
    (multiple-value-bind (cht slopes intercepts) (%make-sample 100 #'< minimum)
      (loop for x from -50 to 50
            do (let ((min1 (cht-increasing-get cht x))
                     (min2 (%optimize cht x slopes intercepts minimum)))
                 (assert (= min1 min2)))))
    ;; monotone decreasing query
    (multiple-value-bind (cht slopes intercepts) (%make-sample 100 #'> minimum)
      (loop for x from 50 downto -50
            do (let ((min1 (cht-decreasing-get cht x))
                     (min2 (%optimize cht x slopes intercepts minimum)))
                 (assert (= min1 min2)))))))
