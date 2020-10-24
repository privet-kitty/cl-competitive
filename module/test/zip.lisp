(defpackage :cp/test/zip
  (:use :cl :fiveam :cp/zip)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/zip)
(in-suite base-suite)

(test zip
  (is (equalp '((1 #\a) (2 #\b)) (zip 'list '(1 2 3) #(#\a #\b))))
  (is (equalp '((1 #\a 1) (2 #\b 2)) (zip 'list '(1 2 3) #(#\a #\b) '(1 2 3 4))))
  (is (null (zip 'list)))
  (is (typep (zip '(simple-array t (*)) '(1 2 3) #(1 2 3 4)) '(array t (*))))
  (is (typep (zip 'list '(1 2 3) #(1 2 3 4)) 'list)))
