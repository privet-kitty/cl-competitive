(defpackage :cp/test/multiset
  (:use :cl :fiveam :cp/multiset :cp/bisect)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multiset)
(in-suite base-suite)

(test multiset/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dotimes (_ 20)
      (let (mset
            (vector (make-array 0 :element-type 'fixnum)))
        (dotimes (_ 300)
          (ecase (random 5)
            ;; push
            (0 (let ((key (random 15))
                     (count (+ 1 (random 3))))
                 (setq mset (mset-insert mset key :count count :order #'>))
                 (let ((pos (bisect-left vector key :order #'>)))
                   (setq vector (concatenate
                                 '(simple-array fixnum (*))
                                 (subseq vector 0 pos)
                                 (make-array count :initial-element key)
                                 (subseq vector pos))))))
            ;; pop
            (1 (let ((key (random 15)))
                 (is (eql (mset-find mset key :order #'>)
                          (find key vector)))
                 (if (zerop (random 2))
                     (setq mset (mset-delete mset key
                                             :count most-positive-fixnum
                                             :error-p nil
                                             :order #'>)
                           vector (delete key vector))
                     (if (mset-find mset key :order #'>)
                         (progn
                           (mset-pop key mset #'>)
                           (setq vector (delete key vector :count 1)))
                         (signals mset-empty-error
                           (mset-pop key mset #'>))))))
            ;; split and concat
            (2 (let ((key (random 15)))
                 (multiple-value-bind (l r) (mset-split mset key :order #'>)
                   (setq mset (%mset-concat l r))))
             (let ((index (random (+ 1 (length vector)))))
               (multiple-value-bind (l r) (mset-indexed-split mset index)
                 (setq mset (mset-concat l r :order #'>)))))
            ;; search
            (3
             (let ((key (random 15)))
               (is (eql (mset-find mset key :order #'>)
                        (find key vector)))
               (is (eql (mset-count mset key :order #'>)
                        (count key vector)))
               (is (= (mset-size mset) (length vector)))
               (when (> (length vector) 0)
                 (is (= (mset-first mset) (aref vector 0)))
                 (is (= (mset-last mset) (aref vector (- (length vector) 1)))))
               (let ((lpos (mset-position-left mset key :order #'>)))
                 (is (= lpos (bisect-left vector key :order #'>)))
                 (let ((lkey (mset-bisect-left mset key :order #'>)))
                   (if lkey
                       (is (= lkey (aref vector lpos)))
                       (is (= lpos (length vector)))))
                 (let ((lkey-1 (mset-bisect-left-1 mset key :order #'>)))
                   (if lkey-1
                       (is (= lkey-1 (aref vector (- lpos 1))))
                       (is (zerop lpos)))))
               (let ((rpos (mset-position-right mset key :order #'>)))
                 (is (= rpos (bisect-right vector key :order #'>)))
                 (let ((rkey (mset-bisect-right mset key :order #'>)))
                   (if rkey
                       (is (= rkey (aref vector rpos)))
                       (is (= rpos (length vector)))))
                 (let ((rkey-1 (mset-bisect-right-1 mset key :order #'>)))
                   (if rkey-1
                       (is (= rkey-1 (aref vector (- rpos 1))))
                       (is (zerop rpos)))))
               (when (> (length vector) 0)
                 (let ((index (random (length vector))))
                   (is (= (mset-ref mset index) (aref vector index)))))))
            ;; map
            (4
             (let ((i 0))
               (mset-map (lambda (key)
                           (is (= key (aref vector i)))
                           (incf i))
                         mset))
             (let ((counter (make-array 15 :element-type 'fixnum :initial-element 0)))
               (mset-map-run-length
                (lambda (key count)
                  (declare (ignore count))
                  (incf (aref counter key)))
                mset)
               (is (every (lambda (x) (<= x 1)) counter))))))))))

(test mset-unite
  (let ((*test-dribble* nil))
    (dotimes (_ 500)
      (let* ((n1 (random 20))
             (n2 (random 20))
             (counter (make-array 15 :element-type 'fixnum :initial-element 0))
             mset1
             mset2)
        (dotimes (i n1)
          (let ((x (random 15)))
            (incf (aref counter x))
            (mset-push x mset1 #'>)))
        (dotimes (i n2)
          (let ((x (random 15)))
            (incf (aref counter x))
            (mset-push x mset2 #'>)))
        (let ((mset (mset-unite mset1 mset2 :order #'>))
              (prev most-positive-fixnum))
          (mset-map-run-length
           (lambda (key count)
             (decf (aref counter key) count)
             (is (> prev key))
             (setq prev key))
           mset)
          (is (loop for x across counter
                    always (zerop x))))))))
