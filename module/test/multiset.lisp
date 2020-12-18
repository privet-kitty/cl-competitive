(defpackage :cp/test/multiset
  (:use :cl :fiveam :cp/multiset :cp/bisect)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multiset)
(in-suite base-suite)

(test multiset/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dotimes (_ 10)
      (let (mset
            (vector (make-array 0 :element-type 'fixnum)))
        (dotimes (_ 300)
          (ecase (random 5)
            ;; push
            (0 (let ((key (random 15)))
                 (multiset-push key mset #'>)
                 (let ((pos (bisect-left vector key :order #'>)))
                   (setq vector (concatenate '(simple-array fixnum (*))
                                             (subseq vector 0 pos)
                                             (vector key)
                                             (subseq vector pos))))))
            ;; pop
            (1 (let ((key (random 15)))
                 (is (eql (multiset-find mset key :order #'>)
                          (find key vector)))
                 (if (multiset-find mset key :order #'>)
                     (progn
                       (multiset-pop key mset #'>)
                       (setq vector (delete key vector :count 1)))
                     (signals multiset-empty-error
                       (multiset-pop key mset #'>)))))
            ;; split and concat
            (2 (let ((key (random 15)))
                 (multiple-value-bind (l r) (multiset-split mset key :order #'>)
                   (setq mset (multiset-concat l r)))))
            ;; find, count, size, first, last
            (3
             (let ((key (random 15)))
               (is (eql (multiset-find mset key :order #'>)
                        (find key vector)))
               (is (eql (multiset-count mset key :order #'>)
                        (count key vector)))
               (is (= (multiset-size mset) (length vector)))
               (when (> (length vector) 0)
                 (is (= (multiset-first mset) (aref vector 0)))
                 (is (= (multiset-last mset) (aref vector (- (length vector) 1)))))))
            ;; map
            (4
             (let ((i 0))
               (multiset-map (lambda (key)
                               (is (= key (aref vector i)))
                               (incf i))
                             mset)))))))))
