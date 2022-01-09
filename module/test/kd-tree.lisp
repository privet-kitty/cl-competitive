(defpackage :cp/test/kd-tree
  (:use :cl :fiveam :cp/kd-tree)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/kd-tree)
(in-suite base-suite)

(test kd-tree/hand
  (finishes (make-kdtree 0 #'identity #'identity)))
