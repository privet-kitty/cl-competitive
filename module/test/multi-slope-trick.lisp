(defpackage :cp/test/multi-slope-trick
  (:use :cl :fiveam :cp/multi-slope-trick :cp/bisect)
  (:import-from :cp/multi-slope-trick
                #:mset #:%mset-key #:%mset-left #:%mset-right #:%mset-count
                #:%mset-concat #:mset-concat #:mset-split #:mset-indexed-split
                #:mset-insert #:mset-map-run-length #:mset-shift
                #:mset-first #:mset-last #:mset-size #:force-down #:update-size)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multi-slope-trick)
(in-suite base-suite)

(declaim (inline mset-find))
(defun mset-find (mset key)
  "Finds and returns KEY if it exists, otherwise returns NIL. Equality is here
equivalent to 'neither larger nor smaller'."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (unless mset
               (return-from recur nil))
             (force-down mset)
             (cond ((< key (%mset-key mset))
                    (recur (%mset-left mset)))
                   ((< (%mset-key mset) key)
                    (recur (%mset-right mset)))
                   (t key))))
    (recur mset)))

(declaim (inline mset-ref))
(defun mset-count (mset key)
  "Returns the number of KEYs in MSET."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (unless mset
               (return-from recur 0))
             (force-down mset)
             (cond ((< key (%mset-key mset))
                    (recur (%mset-left mset)))
                   ((< (%mset-key mset) key)
                    (recur (%mset-right mset)))
                   (t (%mset-count mset)))))
    (recur mset)))

(test translatable-multiset/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil)
        (counter (make-hash-table :test #'eq)))
    (dotimes (_ 20)
      (let (mset
            (vector (make-array 0 :element-type 'fixnum)))
        (dotimes (_ 300)
          (ecase (random 4)
            ;; push
            (0 (let ((key (random 15))
                     (count (+ 1 (random 3))))
                 (setq mset (mset-insert mset key count))
                 (let ((pos (bisect-left vector key)))
                   (setq vector (concatenate
                                 '(simple-array fixnum (*))
                                 (subseq vector 0 pos)
                                 (make-array count :initial-element key)
                                 (subseq vector pos))))))
            ;; split and concat
            (1
             (let ((key (random 15)))
               (multiple-value-bind (l r) (mset-split mset key)
                 (let ((shiftl (- (random 6) 3))
                       (shiftr (- (random 6) 3)))
                   (when (> shiftl shiftr)
                     (rotatef shiftl shiftr))
                   (when l
                     (mset-shift l shiftl)
                     (loop for i below (mset-size l)
                           do (incf (aref vector i) shiftl)))
                   (when r
                     (mset-shift r shiftr)
                     (loop for i below (mset-size r)
                           do (incf (aref vector (+ (mset-size l) i)) shiftr))))
                 (setq mset (%mset-concat l r))))
             (let ((index (random (+ 1 (length vector)))))
               (multiple-value-bind (l r) (mset-indexed-split mset index)
                 (let ((shiftl (- (random 6) 3))
                       (shiftr (- (random 6) 3)))
                   (when (> shiftl shiftr)
                     (rotatef shiftl shiftr))
                   (when l
                     (mset-shift l shiftl)
                     (loop for i below (mset-size l)
                           do (incf (aref vector i) shiftl)))
                   (when r
                     (mset-shift r shiftr)
                     (loop for i below (mset-size r)
                           do (incf (aref vector (+ (mset-size l) i)) shiftr))))
                 (setq mset (mset-concat l r)))))
            ;; search
            (2
             (let ((key (if (or (zerop (length vector)) (zerop (random 3)))
                            (random 15)
                            (aref vector (random (length vector))))))
               (is (eql (mset-find mset key) (find key vector)))
               (is (eql (mset-count mset key) (count key vector)))
               (is (= (mset-size mset) (length vector)))
               (when (> (length vector) 0)
                 (is (= (mset-first mset) (aref vector 0)))
                 (is (= (mset-last mset) (aref vector (- (length vector) 1)))))))
            ;; map
            (3
             (clrhash counter)
             (let ((prev most-negative-fixnum))
               (mset-map-run-length
                (lambda (key count)
                  (declare (ignore count))
                  (is (< prev key))
                  (setq prev key)
                  (incf (the fixnum (gethash key counter 0))))
                mset))
             (is (loop for x being each hash-value of counter
                       always (<= x 1))))))))))
