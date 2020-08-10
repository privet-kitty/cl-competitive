(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../rolling-hash62.lisp"))

(use-package :test-util)

(declaim (notinline rhash-query rhash-vector-hash rhash-concat))

;; (with-test (:name rolling-hash/fixed-mod)
;;   (let ((rhash1 (make-rhash "asddfddfd" :key (lambda (x) (+ 1 (char-code x))))))
;;     (assert (= (rhash-query rhash1 2 6) (rhash-query rhash1 5 9)))
;;     (assert (= (rhash-query rhash1 2 2) (rhash-query rhash1 5 5)))
;;     (assert (/= (rhash-query rhash1 2 6) (rhash-query rhash1 3 7)))
;;     (assert (= (rhash-concat rhash1 (rhash-query rhash1 0 2) (rhash-query rhash1 5 8) 3)
;;                (rhash-query rhash1 0 5)))
;;     (signals error (make-rhash "error" :mod1 1000000006 :mod2 1000000009 :base1 1729 :base2 10007))
;;     (make-rhash "no error" :mod1 17 :mod2 1000000009 :base1 11 :base2 10007)
;;     (signals error (make-rhash "error" :mod1 17 :mod2 1000000009 :base1 19 :base2 10007))
;;     (signals error (make-rhash "error" :mod1 17 :mod2 1000000009 :base1 17 :base2 10007))
;;     (signals error (make-rhash "error" :mod1 17 :mod2 1000000009 :base1 0 :base2 10007))

    
;;     ;; longest common prefix
;;     (assert (= 0 (rhash-get-lcp rhash1 0 rhash1 3)))
;;     (assert (= 1 (rhash-get-lcp rhash1 2 rhash1 3)))
;;     (assert (= 0 (rhash-get-lcp rhash1 2 rhash1 4)))
;;     (assert (= 4 (rhash-get-lcp rhash1 2 rhash1 5)))
;;     (assert (= 7 (rhash-get-lcp rhash1 2 rhash1 2)))))

(with-test (:name rolling-hash)
  (let ((rhash1 (make-rhash "asddfddfd" :key (lambda (x) (+ 1 (char-code x))))))
    (assert (= (rhash-query rhash1 2 6) (rhash-query rhash1 5 9)))
    (assert (= (rhash-query rhash1 2 2) (rhash-query rhash1 5 5)))
    (assert (/= (rhash-query rhash1 2 6) (rhash-query rhash1 3 7)))
    (assert (= (rhash-concat rhash1 (rhash-query rhash1 0 2) (rhash-query rhash1 5 8) 3)
               (rhash-query rhash1 0 5)))

    (assert (= (position +rhash-mod1+ *moduli-table*)
               (position +rhash-base1+ *base-table*)))
    (assert (= (position +rhash-mod2+ *moduli-table*)
               (position +rhash-base2+ *base-table*)))

    (assert (rhash-query (make-rhash "") 0 0))

    ;; hash code of a given sequence
    (assert (= (rhash-vector-hash "sddf" :key (lambda (x) (+ 1 (char-code x))))
               (rhash-query rhash1 1 5)))
    (assert (/= (rhash-vector-hash "sddf")
                (rhash-query rhash1 1 5)))
    (assert (zerop (rhash-vector-hash "" :key (lambda (x) (+ 1 (char-code x))))))
    
    ;; longest common prefix
    (assert (= 0 (rhash-get-lcp rhash1 0 rhash1 3)))
    (assert (= 1 (rhash-get-lcp rhash1 2 rhash1 3)))
    (assert (= 0 (rhash-get-lcp rhash1 2 rhash1 4)))
    (assert (= 4 (rhash-get-lcp rhash1 2 rhash1 5)))
    (assert (= 7 (rhash-get-lcp rhash1 2 rhash1 2))))

  ;; zero
  (let ((rhash (make-rhash #*00000 :key #'identity)))
    (loop for l from 0 to 5
          do (loop for r from l to 5
                   do (assert (zerop (rhash-query rhash l r)))))
    (assert (zerop (rhash-vector-hash #*0000 :key #'identity)))))
