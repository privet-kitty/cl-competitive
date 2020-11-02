(defpackage :cp/test/complex-geometry
  (:use :cl :fiveam :cp/complex-geometry)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/complex-geometry)
(in-suite base-suite)

(test cross*
  (is (= 0 (cross* #c(1 3) #c(2 6))))
  (is (= -6 (cross* #c(5 7) #c(13 17)))))

