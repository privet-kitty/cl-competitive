(defpackage :cp/test/hackenbush
  (:use :cl :fiveam :cp/hackenbush)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/hackenbush #:add-blue #:add-red))
(in-package :cp/test/hackenbush)
(in-suite base-suite)

(test add-blue
  (is (= 1 (add-blue 0))))

(test add-red
  (is (= -1 (add-red 0))))

(test calc-game-value-for-stalk
  (is (= 11/8 (calc-game-value-for-stalk #(1 1 -1 -1 1))))
  (is (= -37/16 (calc-game-value-for-stalk #(-1 -1 -1 1 1 -1 1)))))
