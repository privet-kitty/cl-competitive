(defpackage :cp/test/stirling2
  (:use :cl :fiveam :cp/stirling2)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/stirling2)
(in-suite base-suite)

(test stirling2
  (is (equalp #2A((1 0 0 0 0 0 0 0)
                  (0 1 0 0 0 0 0 0)
                  (0 1 1 0 0 0 0 0)
                  (0 1 3 1 0 0 0 0)
                  (0 1 7 6 1 0 0 0)
                  (0 1 15 25 10 1 0 0)
                  (0 1 31 90 65 15 1 0)
                  (0 1 63 301 350 140 21 1))
              (make-stirling2-table 8 8 1000000007))))
