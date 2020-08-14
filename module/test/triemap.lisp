(defpackage :cp/test/triemap
  (:use :cl :fiveam :cp/triemap)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/triemap)
(in-suite base-suite)

(test triemap
  (let ((triemap (make-triemap)))
    (triemap-add! triemap "abra" 1)
    (triemap-add! triemap "abrac" 2)
    (triemap-add! triemap "rac" 3)
    (triemap-add! triemap "racad" 4)
    (is (= 1 (triemap-get triemap "abra")))
    (is (null (triemap-get triemap "ab")))
    (is (null (triemap-get triemap "")))
    (triemap-add! triemap "" 5)
    (is (= 5 (triemap-get triemap "")))
    (is (equal '(5 2) (multiple-value-list (triemap-query-longest triemap "abracadabra"))))
    (let ((res #(-1 -1 5 -1 -1 3 -1 4)))
      (triemap-query triemap "abracadabra"
                     (lambda (pos value)
                       (is (= (aref res pos) value)))
                     :start 2))))
