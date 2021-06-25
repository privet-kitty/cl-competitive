(defpackage :cp/test/implicit-treap
  (:use :cl :fiveam :cp/implicit-treap)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/implicit-treap
                #:%itreap-value #:%itreap-priority #:%itreap-left #:%itreap-right
                #:%itreap-count #:%itreap-accumulator #:%itreap-lazy #:%itreap-reversed
                #:%make-itreap #:force-down #:force-up))
(in-package :cp/test/implicit-treap)
(in-suite base-suite)

(defun copy-itreap (itreap)
  "For development. Recursively copies the whole ITREAPs."
  (if (null itreap)
      nil
      (%make-itreap (%itreap-value itreap)
                    (%itreap-priority itreap)
                    :left (copy-itreap (%itreap-left itreap))
                    :right (copy-itreap (%itreap-right itreap))
                    :count (%itreap-count itreap)
                    :accumulator (%itreap-accumulator itreap)
                    :lazy (%itreap-lazy itreap)
                    :reversed (%itreap-reversed itreap))))

(defun itreap-sane-p (itreap)
  (labels ((priority* (itreap)
             (if (null itreap) 0 (%itreap-priority itreap))))
    (declare (inline priority*))
    (if (null itreap)
        t
        (and (> (%itreap-priority itreap) (priority* (%itreap-left itreap)))
             (> (%itreap-priority itreap) (priority* (%itreap-right itreap)))
             (itreap-sane-p (%itreap-left itreap))
             (itreap-sane-p (%itreap-right itreap))))))

(defun itreap-list (itreap)
  (let (res)
    (labels ((recur (itreap)
               (when itreap
                 (force-down itreap)
                 (recur (%itreap-left itreap))
                 (push (%itreap-value itreap) res)
                 (recur (%itreap-right itreap))
                 (force-up itreap))))
      (recur itreap)
      (reverse res))))

(defun itreap-tree (itreap)
  (if (null itreap)
      nil
      (cons (%itreap-value itreap)
            (cons (itreap-tree (%itreap-left itreap))
                  (itreap-tree (%itreap-right itreap))))))


(test implicit-treap/sanity
  (is-true (loop for i below 100
                 always (itreap-sane-p (make-itreap i)))))

(test imliticit-treap/make-itreap
  (let ((itreap (make-itreap 10 :initial-element 3)))
    (dotimes (i 10)
      (is (= 3 (itreap-ref itreap i))))
    (dotimes (l 10)
      (loop for r from l to 10
            when (= l r)
            do (is (= most-positive-fixnum (itreap-fold itreap l r)))
            else
            do (is (= 3 (itreap-fold itreap l r)))))))

(test implicit-treap/map
  (let* ((vec #(1 2 3 4 5 6))
         (itreap (make-itreap 6 :initial-contents vec)))
    (dotimes (l (+ 1 (length vec)))
      (loop for r from l to (length vec)
            for subvec = (make-array 0 :fill-pointer 0)
            do (itreap-map itreap
                           (lambda (x) (vector-push-extend x subvec))
                           l r)
               (is (equalp subvec (subseq vec l r)))))))

(test implicit-treap/invalid-itreap-index-error
  (let ((itreap (itreap 3 5 2 4 1)))
    (signals invalid-itreap-index-error (itreap-ref itreap 5))
    (signals invalid-itreap-index-error (itreap-insert itreap 6 10))
    (signals invalid-itreap-index-error (itreap-delete itreap 6))
    (signals invalid-itreap-index-error (itreap-fold itreap 4 6))
    (signals invalid-itreap-index-error (itreap-update itreap 10 4 6))
    (signals invalid-itreap-index-error (itreap-reverse itreap 4 6))
    (signals invalid-itreap-index-error (itreap-max-right itreap (constantly t) 6))
    (signals invalid-itreap-index-error (itreap-min-left itreap (constantly t) 6))))

(test implicit-treap/insert
  (declare (notinline itreap-insert))
  (is (equal '(10 1 2 3 4 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 0 10))))
  (is (equal '(1 10 2 3 4 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 1 10))))
  (is (equal '(1 2 10 3 4 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 2 10))))
  (is (equal '(1 2 3 10 4 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 3 10))))
  (is (equal '(1 2 3 4 10 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 4 10))))
  (is (equal '(1 2 3 4 5 10 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 5 10))))
  (is (equal '(1 2 3 4 5 6 10)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 6 10))))
  (is (= 10 (itreap-fold (itreap-insert (itreap 1 2 3 4 5 6) 3 10) 3 4)))
  (signals invalid-itreap-index-error
    (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 7 10)))
  (is (equal '(10) (itreap-list (itreap-insert nil 0 10)))))

(test implicit-treap/delete
  (declare (notinline itreap-delete itreap-fold))
  (is (equal '(2 3 4 5 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 0))))
  (is (equal '(1 3 4 5 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 1))))
  (is (equal '(1 2 4 5 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 2))))
  (is (equal '(1 2 3 5 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 3))))
  (is (equal '(1 2 3 4 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 4))))
  (is (equal '(1 2 3 4 5)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 5))))
  (signals invalid-itreap-index-error (itreap-delete (itreap 1 2 3 4 5 6) 6))
  (signals invalid-itreap-index-error (itreap-delete nil 0))
  (let ((itreap (itreap 1 2 3 2 1 2 7)))
    (is (= 1 (itreap-fold itreap 1 5)))
    (setq itreap (itreap-delete itreap 4))
    (is (= 2 (itreap-fold itreap 1 5)))))

(test implicit-treap
  (declare (notinline itreap-fold itreap-ref itreap-update))
  (dotimes (i 10)
    (let ((itreap1 (itreap 1 2 3 2 1 2 7)))
      (is (equal '(1 2 3 2 1 2 7) (itreap-list itreap1)))
      ;; ref and fold
      (is (= 7 (itreap-ref itreap1 6)))
      (is (= 1 (itreap-fold itreap1 0 7)))
      (is (= 2 (itreap-fold itreap1 5 7)))
      (is (= 2 (itreap-fold itreap1 1 3)))
      ;; point update
      (setf (itreap-ref itreap1 1) 0)
      (is (= 0 (itreap-ref itreap1 1)))
      (is (= most-positive-fixnum (itreap-fold itreap1 1 1)))
      (is (= 0 (itreap-fold itreap1 0 3)))
      (is (= 0 (itreap-fold itreap1 0 7)))
      (is (= 1 (itreap-fold itreap1 2 7)))
      (is (= 2 (itreap-fold itreap1 2 4)))
      ;; invalid index error
      (signals invalid-itreap-index-error (itreap-fold itreap1 2 8))
      (signals invalid-itreap-index-error (itreap-fold itreap1 2 1))
      (signals invalid-itreap-index-error (itreap-ref itreap1 8))
      (signals invalid-itreap-index-error (setf (itreap-ref itreap1 8) 5))
      ;; range update
      (setf itreap1 (itreap 1 2 3 2 1 2 7))
      (setf itreap1 (itreap-update itreap1 -10 3 5))
      (is (equal '(1 2 3 -8 -9 2 7) (itreap-list itreap1)))
      (is (= -8 (itreap-fold itreap1 2 4)))
      (is (= 3 (itreap-fold itreap1 2 3)))
      ;; range update (for null range)
      (setf itreap1 (itreap-update itreap1 -10 3 3))
      (is (equal '(1 2 3 -8 -9 2 7) (itreap-list itreap1)))
      (is (= -8 (itreap-fold itreap1 2 4)))
      (is (= 3 (itreap-fold itreap1 2 3))))))

(defun %make (list)
  (let ((len (length list))
        nodes
        itreap)
    (dotimes (i len)
      (let* ((idx (random (- len i)))
             (elm (nth idx list)))
        (push (cons elm idx) nodes)
        (setq list
              (append (subseq list 0 idx)
                      (subseq list (+ idx 1))))))
    (loop for (elm . idx) in nodes
          do (itreap-push itreap idx elm))
    itreap))

(test implicit-treap-max-right
  (declare (notinline itreap-max-right))
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (list '(5 2 4 2 1 4 2 1 -1)))
    (is (= 0 (itreap-max-right (%make list) (lambda (x) (> x 6)))))
    (is (= 0 (itreap-max-right (%make list) (lambda (x) (> x 5)))))
    (is (= 1 (itreap-max-right (%make list) (lambda (x) (> x 4)))))
    (is (= 1 (itreap-max-right (%make list) (lambda (x) (> x 3)))))
    (is (= 1 (itreap-max-right (%make list) (lambda (x) (> x 2)))))
    (is (= 4 (itreap-max-right (%make list) (lambda (x) (> x 1)))))
    (is (= 8 (itreap-max-right (%make list) (lambda (x) (> x 0)))))
    (is (= 8 (itreap-max-right (%make list) (lambda (x) (> x -1)))))
    (is (= 9 (itreap-max-right (%make list) (lambda (x) (> x -2)))))
    
    ;; START arg
    (is (= 3 (itreap-max-right (%make list) (lambda (x) (> x 6)) 3)))
    (is (= 3 (itreap-max-right (%make list) (lambda (x) (> x 5)) 3)))
    (is (= 3 (itreap-max-right (%make list) (lambda (x) (> x 4)) 3)))
    (is (= 3 (itreap-max-right (%make list) (lambda (x) (> x 3)) 3)))
    (is (= 3 (itreap-max-right (%make list) (lambda (x) (> x 2)) 3)))
    (is (= 4 (itreap-max-right (%make list) (lambda (x) (> x 1)) 3)))
    (is (= 8 (itreap-max-right (%make list) (lambda (x) (> x 0)) 3)))
    (is (= 8 (itreap-max-right (%make list) (lambda (x) (> x -1)) 3)))
    (is (= 9 (itreap-max-right (%make list) (lambda (x) (> x -2)) 3)))

    (is (= 8 (itreap-max-right (%make list) (lambda (x) (> x 6)) 8)))
    (is (= 8 (itreap-max-right (%make list) (lambda (x) (> x -1)) 8)))
    (is (= 9 (itreap-max-right (%make list) (lambda (x) (> x -2)) 8)))

    (is (= 9 (itreap-max-right (%make list) (lambda (x) (> x -10)) 9)))
    (is (= 9 (itreap-max-right (%make list) (lambda (x) (> x 10)) 9)))

    (signals invalid-itreap-index-error
      (itreap-max-right (%make list) (lambda (x) (> x 10)) 10))

    ;; null case
    (is (zerop (itreap-max-right nil (lambda (x) (> x 10)))))))

(test implicit-treap-min-left
  (declare (notinline itreap-min-left))
  (let ((list '(5 -3 -3 2 -1 4 2 1 3)))
    (is (= 9 (itreap-min-left (%make list) (lambda (x) (> x 6)))))
    (is (= 9 (itreap-min-left (%make list) (lambda (x) (> x 5)))))
    (is (= 9 (itreap-min-left (%make list) (lambda (x) (> x 4)))))
    (is (= 9 (itreap-min-left (%make list) (lambda (x) (> x 3)))))
    (is (= 8 (itreap-min-left (%make list) (lambda (x) (> x 2)))))
    (is (= 8 (itreap-min-left (%make list) (lambda (x) (> x 1)))))
    (is (= 5 (itreap-min-left (%make list) (lambda (x) (> x 0)))))
    (is (= 5 (itreap-min-left (%make list) (lambda (x) (> x -1)))))
    (is (= 3 (itreap-min-left (%make list) (lambda (x) (> x -2)))))
    (is (= 3 (itreap-min-left (%make list) (lambda (x) (> x -3)))))
    (is (= 0 (itreap-min-left (%make list) (lambda (x) (> x -4)))))
    
    ;; END arg
    (is (= 6 (itreap-min-left (%make list) (lambda (x) (> x 6)) 6)))
    (is (= 6 (itreap-min-left (%make list) (lambda (x) (> x 5)) 6)))
    (is (= 6 (itreap-min-left (%make list) (lambda (x) (> x 4)) 6)))
    (is (= 5 (itreap-min-left (%make list) (lambda (x) (> x 3)) 6)))
    (is (= 5 (itreap-min-left (%make list) (lambda (x) (> x 2)) 6)))
    (is (= 5 (itreap-min-left (%make list) (lambda (x) (> x 1)) 6)))
    (is (= 5 (itreap-min-left (%make list) (lambda (x) (> x 0)) 6)))
    (is (= 5 (itreap-min-left (%make list) (lambda (x) (> x -1)) 6)))
    (is (= 3 (itreap-min-left (%make list) (lambda (x) (> x -2)) 6)))
    (is (= 3 (itreap-min-left (%make list) (lambda (x) (> x -3)) 6)))
    (is (= 0 (itreap-min-left (%make list) (lambda (x) (> x -4)) 6)))

    (is (= 4 (itreap-min-left (%make list) (lambda (x) (> x 2)) 4)))
    (is (= 2 (itreap-min-left (%make list) (lambda (x) (> x -3)) 2)))
    (is (= 0 (itreap-min-left (%make list) (lambda (x) (> x -4)) 2)))

    (is (= 0 (itreap-min-left (%make list) (lambda (x) (> x 10)) 0)))

    (signals invalid-itreap-index-error
      (itreap-min-left (%make list) (lambda (x) (> x 10)) 10))

    ;; null case
    (is (zerop (itreap-min-left nil (lambda (x) (> x 10)))))))

(test implicit-treap/sorted
  (declare (notinline itreap-bisect-left itreap-bisect-right))
  (let ((list '(3 3 2 2 2 2 1 1 1)))
    (is (= 6 (itreap-bisect-left (%make list) 1 #'>)))
    (is (= 9 (itreap-bisect-left (%make list) 0 #'>)))
    (is (= 0 (itreap-bisect-left (%make list) 4 #'>)))
    (is (= 0 (itreap-bisect-right (%make list) 4 #'>)))
    (is (= 2 (itreap-bisect-right (%make list) 3 #'>)))
    (is (= 9 (itreap-bisect-right (%make list) 1 #'>)))))

(test implicit-treap/reverse
  (declare (notinline itreap-reverse))
  (is (null (itreap-reverse nil 0 0))))

(test implicit-treap/push-pop
  (declare (notinline itreap-ref make-itreap itreap-delete itreap-insert))
  (let ((vector (vector nil (make-itreap 3  :initial-contents #(10 20 30)))))
    (itreap-push (aref vector 1) 1 1000)
    (is (itreap-ref (aref vector 1) 0) 10)
    (is (itreap-ref (aref vector 1) 1) 1000)
    (is (itreap-ref (aref vector 1) 2) 20)
    (is (itreap-ref (aref vector 1) 3) 30)
    (is (= 20 (itreap-pop (aref vector 1) 2)))
    (is (itreap-ref (aref vector 1) 2) 30)))

(test implicit-treap/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (_ 200)
      (let* ((len (random 15))
             (vec (make-array len :element-type 'fixnum))
             vec2)
        (declare ((simple-array fixnum (*)) vec)
                 ((or null (simple-array fixnum (*))) vec2))
        (dotimes (i len)
          (let ((val (random 100)))
            (setf (aref vec i) val)))
        (let ((itreap (make-itreap len :initial-contents vec))
              itreap2)
          (dotimes (_ 400)
            (ecase (random 10)
              ;; insert
              (0 (let ((index (random (+ (length vec) 1)))
                       (val (random 100)))
                   (itreap-push itreap index val)
                   (setq vec (concatenate '(simple-array fixnum (*))
                                          (subseq vec 0 index)
                                          (vector val)
                                          (subseq vec index)))))
              ;; delete
              (1 (unless (zerop (length vec))
                   (let ((index (random (length vec))))
                     (itreap-pop itreap index)
                     (setq vec (concatenate '(simple-array fixnum (*))
                                            (subseq vec 0 index)
                                            (subseq vec (+ index 1)))))))
              ;; point update
              (2 (unless (zerop (length vec))
                   (let ((index (random (length vec)))
                         (new-val (random 100)))
                     (setf (aref vec index) new-val
                           (itreap-ref itreap index) new-val))))
              ;; range min
              ((3 4) (let ((l (random (+ 1 (length vec))))
                           (r (random (+ 1 (length vec)))))
                       (when (> l r)
                         (rotatef l r))
                       (is (= (itreap-fold itreap l r)
                              (loop with min = most-positive-fixnum
                                    for i from l below r
                                    do (setq min (min min (aref vec i)))
                                    finally (return min))))))
              ;; range update
              (5 (let ((l (random (+ 1 (length vec))))
                       (r (random (+ 1 (length vec))))
                       (delta (- (random 20) 10)))
                   (when (> l r)
                     (rotatef l r))
                   (itreap-update itreap delta l r)
                   (loop for i from l below r
                         do (incf (aref vec i) delta))))
              ;; max-right
              (6 (let ((start (random (+ 1 (length vec))))
                       (threshold (- (random 200) 100)))
                   (is (= (itreap-max-right itreap (lambda (x) (>= x threshold)) start)
                          (loop with min = most-positive-fixnum
                                for i from start below (length vec)
                                do (setq min (min min (aref vec i)))
                                while (>= min threshold)
                                finally (return i))))))
              ;; min-left
              (7 (let ((end (random (+ 1 (length vec))))
                       (threshold (- (random 200) 100)))
                   (is (= (itreap-min-left itreap (lambda (x) (>= x threshold)) end)
                          (loop with min = most-positive-fixnum
                                for i from (- end 1) downto 0 
                                do (setq min (min min (aref vec i)))
                                while (>= min threshold)
                                finally (return (+ i 1)))))))
              ;; reverse
              (8 (let ((l (random (+ 1 (length vec))))
                       (r (random (+ 1 (length vec)))))
                   (when (> l r)
                     (rotatef l r))
                   (setq itreap (itreap-reverse itreap l r))
                   (setq vec (concatenate '(simple-array fixnum (*))
                                          (subseq vec 0 l)
                                          (nreverse (subseq vec l r))
                                          (subseq vec r)))))
              ;; merge/split
              (9 (if itreap2
                     (setq itreap (itreap-merge itreap2 itreap)
                           vec (concatenate '(simple-array fixnum (*))
                                            vec2 vec)
                           itreap2 nil
                           vec2 nil)
                     (let ((index (random (+ 1 (length vec)))))
                       (multiple-value-bind (itreap-l itreap-r)
                           (itreap-split itreap index)
                         (setq itreap itreap-l
                               itreap2 itreap-r))
                       (setq vec2 (subseq vec index)
                             vec (subseq vec 0 index))))))))))))
