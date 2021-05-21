(defpackage :cp/test/sliding-window-fixed
  (:use :cl :fiveam :cp/sliding-window-fixed)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/sliding-window-fixed)
(in-suite base-suite)

(test sliding-window-fixed
  (declare (notinline sliding-window-fixed))
  (is (equalp #(1 1 2 2 3 -1) (sliding-window-fixed #(5 1 4 2 3 5 7 -1) 3 #'<)))
  (is (equalp #(5 4 4 5 7 7) (sliding-window-fixed #(5 1 4 2 3 5 7 -1) 3 #'>)))
  (is (equalp #(-1) (sliding-window-fixed #(5 1 4 2 3 5 7 -1) 8 #'<)))
  (signals error (sliding-window-fixed #(5 1 4 2 3 5 7 -1) 9 #'>))

  (is (equalp #(10) (sliding-window-fixed #(10) 1 #'<)))
  (is (equalp #(10) (sliding-window-fixed #(10) 1 #'>)))
  (signals error (sliding-window-fixed #() 0 #'<)))
