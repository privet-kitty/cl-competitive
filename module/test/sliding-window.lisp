(defpackage :cp/test/sliding-window
  (:use :cl :fiveam :cp/sliding-window)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/sliding-window)
(in-suite base-suite)

(test sliding-window
  (is (equalp #(1 1 2 2 3 -1) (calc-window-opt #(5 1 4 2 3 5 7 -1) 3 #'<)))
  (is (equalp #(5 4 4 5 7 7) (calc-window-opt #(5 1 4 2 3 5 7 -1) 3 #'>)))
  (is (equalp #(-1) (calc-window-opt #(5 1 4 2 3 5 7 -1) 8 #'<)))
  (signals error (calc-window-opt #(5 1 4 2 3 5 7 -1) 9 #'>))

  (is (equalp #(10) (calc-window-opt #(10) 1 #'<)))
  (is (equalp #(10) (calc-window-opt #(10) 1 #'>)))
  (locally (declare (sb-ext:muffle-conditions warning))
    (signals error (calc-window-opt #() 0 #'<))))
