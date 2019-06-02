(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../fft.lisp"))

(use-package :test-util)

(with-test (:name logreverse)
  (assert (= #b11010101 (logreverse #b10101011 8)))
  (assert (= #b1101 (logreverse #b10101011 4)))
  (assert (= #b110 (logreverse #b10101011 3)))
  (assert (= #b110 (logreverse #b10101011 3)))
  (assert (= #b1010101101010100101010011000110011101011000111110000101011010010
             (logreverse #b0100101101010000111110001101011100110001100101010010101011010101 64)))
  (assert (= #b10101011010101001010100110001100111010110001111100001010110100
             (logreverse #b0100101101010000111110001101011100110001100101010010101011010101 62))))
