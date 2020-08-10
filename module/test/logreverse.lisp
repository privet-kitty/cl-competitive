(defpackage :cp/test/logreverse
  (:use :cl :fiveam :cp/logreverse)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/logreverse)
(in-suite base-suite)

(test logreverse
  (is (= #b11010101 (logreverse #b10101011 8)))
  (is (= #b1101 (logreverse #b10101011 4)))
  (is (= #b110 (logreverse #b10101011 3)))
  (is (= #b110 (logreverse #b10101011 3)))
  (is (= #b1010101101010100101010011000110011101011000111110000101011010010
         (logreverse #b0100101101010000111110001101011100110001100101010010101011010101 64)))
  (is (= #b10101011010101001010100110001100111010110001111100001010110100
         (logreverse #b0100101101010000111110001101011100110001100101010010101011010101 62))))
