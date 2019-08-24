(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../rolling-hash62.lisp"))

(use-package :test-util)

(declaim (notinline rhash-query rhash-vector-hash rhash-concat))

(with-test (:name rolling-hash/fixed-mod)
  (let ((rhash1 (make-rhash "asddfddfd" :mod1 1000000007 :base1 1729
                                        :mod2 1000000009 :base2 10007
                                        :key (lambda (x) (+ 1 (char-code x))))))
    (assert (= (rhash-query rhash1 2 6) (rhash-query rhash1 5 9)))
    (assert (= (rhash-query rhash1 2 2) (rhash-query rhash1 5 5)))
    (assert (/= (rhash-query rhash1 2 6) (rhash-query rhash1 3 7)))
    (assert (= (rhash-concat rhash1 (rhash-query rhash1 0 2) (rhash-query rhash1 5 8) 3)
               (rhash-query rhash1 0 5)))
    (signals error (make-rhash "error" :mod1 1000000006 :mod2 1000000009 :base1 1729 :base2 10007))
    (make-rhash "no error" :mod1 17 :mod2 1000000009 :base1 11 :base2 10007)
    (signals error (make-rhash "error" :mod1 17 :mod2 1000000009 :base1 19 :base2 10007))
    (signals error (make-rhash "error" :mod1 17 :mod2 1000000009 :base1 17 :base2 10007))
    (signals error (make-rhash "error" :mod1 17 :mod2 1000000009 :base1 0 :base2 10007))

    ;; hash code of a given sequence
    (assert (= (rhash-vector-hash rhash1 "sddf" :key (lambda (x) (+ 1 (char-code x))))
               (rhash-query rhash1 1 5)))
    (assert (zerop (rhash-vector-hash rhash1 "" :key (lambda (x) (+ 1 (char-code x))))))
    
    ;; longest common prefix
    (assert (= 0 (rhash-get-lcp rhash1 0 rhash1 3)))
    (assert (= 1 (rhash-get-lcp rhash1 2 rhash1 3)))
    (assert (= 0 (rhash-get-lcp rhash1 2 rhash1 4)))
    (assert (= 4 (rhash-get-lcp rhash1 2 rhash1 5)))
    (assert (= 7 (rhash-get-lcp rhash1 2 rhash1 2)))))

(with-test (:name rolling-hash/random-mod)
  (let ((rhash1 (make-rhash "asddfddfd")))
    (assert (= (rhash-query rhash1 2 6) (rhash-query rhash1 5 9)))
    (assert (= (rhash-query rhash1 2 2) (rhash-query rhash1 5 5)))
    (assert (/= (rhash-query rhash1 2 6) (rhash-query rhash1 3 7)))
    (assert (= (rhash-concat rhash1 (rhash-query rhash1 0 2) (rhash-query rhash1 5 8) 3)
               (rhash-query rhash1 0 5)))

    (let ((mod1 (rhash-mod1 rhash1))
          (mod2 (rhash-mod2 rhash1))
          (base1 (aref (rhash-powers1 rhash1) 1))
          (base2 (aref (rhash-powers2 rhash1) 1)))
      (assert (= (position mod1 *moduli-table*)
                 (position base1 *base-table*)))
      (assert (= (position mod2 *moduli-table*)
                 (position base2 *base-table*))))

    (make-rhash "")
    (let ((*random-state* (seed-random-state 0)))
      (dotimes (i 1000)
        (let ((rhash (make-rhash "a")))
          (assert (/= (rhash-mod1 rhash) (rhash-mod2 rhash))))))
    
    ;; longest common prefix
    (assert (= 0 (rhash-get-lcp rhash1 0 rhash1 3)))
    (assert (= 1 (rhash-get-lcp rhash1 2 rhash1 3)))
    (assert (= 0 (rhash-get-lcp rhash1 2 rhash1 4)))
    (assert (= 4 (rhash-get-lcp rhash1 2 rhash1 5)))
    (assert (= 7 (rhash-get-lcp rhash1 2 rhash1 2)))))
