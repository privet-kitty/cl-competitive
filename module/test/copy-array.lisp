(defpackage :cp/test/copy-array
  (:use :cl :fiveam :cp/copy-array)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/copy-array)
(in-suite base-suite)

(test copy-array/hand
  (let* ((arr #2a((1 2 3) (4 5 6.0)))
         (copied (copy-array arr)))
    (is (equalp copied #2a((1 2 3) (4 5 6.0))))
    (is (not (eq arr copied))))
  (let* ((arr (make-array '(2 3)
                          :element-type 'fixnum
                          :initial-contents '((1 2 3) (4 5 6))
                          :adjustable t))
         (copied (copy-array arr)))
    (is (equalp copied #2a((1 2 3) (4 5 6))))
    (is (not (eq arr copied)))
    (is (typep arr '(array fixnum (2 3))))
    (is (typep copied '(simple-array fixnum (2 3)))))
  (let* ((arr (make-array 2 :element-type 'base-char :fill-pointer 1 :initial-element #\a))
         (copied (copy-array arr)))
    (is (equalp copied "aa"))
    (is (not (eq arr copied)))
    (is (typep arr '(base-string 2)))
    (is (typep copied '(simple-base-string 2)))))

;; (defun copy-array-func (arr1 arr2 arr3)
;;   (declare ((simple-array fixnum (3 4)) arr1)
;;            ((and (array fixnum *) (not simple-array)) arr2)
;;            ((or integer (simple-array fixnum (*)) array) arr3))
;;   (values (copy-array arr1) (copy-array arr2) (copy-array arr3)))
