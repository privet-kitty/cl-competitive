(defpackage :cp/test/rolling-hash62
  (:use :cl :fiveam :cp/rolling-hash62)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/rolling-hash62)
(in-suite base-suite)

(test rolling-hash62
  (declare (notinline make-rhash rhash-query rhash-concat rhash-get-lcp rhash-vector-hash))
  (let ((rhash1 (make-rhash "asddfddfd" :key (lambda (x) (+ 1 (char-code x))))))
    (is (= (rhash-query rhash1 2 6) (rhash-query rhash1 5 9)))
    (is (= (rhash-query rhash1 2 2) (rhash-query rhash1 5 5)))
    (is (/= (rhash-query rhash1 2 6) (rhash-query rhash1 3 7)))
    (is (= (rhash-concat rhash1 (rhash-query rhash1 0 2) (rhash-query rhash1 5 8) 3)
           (rhash-query rhash1 0 5)))

    (is (= (position +rhash-mod1+ *moduli-table*)
           (position +rhash-base1+ *base-table*)))
    (is (= (position +rhash-mod2+ *moduli-table*)
           (position +rhash-base2+ *base-table*)))

    (is (rhash-query (make-rhash "") 0 0))

    ;; hash code of a given sequence
    (is (= (rhash-vector-hash "sddf" :key (lambda (x) (+ 1 (char-code x))))
           (rhash-query rhash1 1 5)))
    (is (/= (rhash-vector-hash "sddf")
            (rhash-query rhash1 1 5)))
    (is (zerop (rhash-vector-hash "" :key (lambda (x) (+ 1 (char-code x))))))
    
    ;; longest common prefix
    (is (= 0 (rhash-get-lcp rhash1 0 rhash1 3)))
    (is (= 1 (rhash-get-lcp rhash1 2 rhash1 3)))
    (is (= 0 (rhash-get-lcp rhash1 2 rhash1 4)))
    (is (= 4 (rhash-get-lcp rhash1 2 rhash1 5)))
    (is (= 7 (rhash-get-lcp rhash1 2 rhash1 2))))

  ;; zero
  (let ((rhash (make-rhash #*00000 :key #'identity)))
    (loop for l from 0 to 5
          do (loop for r from l to 5
                   do (is (zerop (rhash-query rhash l r)))))
    (is (zerop (rhash-vector-hash #*0000 :key #'identity)))))
