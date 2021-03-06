;;;
;;; Maximum cardinality matching in general unweighted graph (Gabow-Edmonds)
;;;
;;; Reference:
;;; https://qiita.com/Kutimoti_T/items/5b579773e0a24d650bdf (Japanese)
;;; https://web.archive.org/web/20170620142342/https://min-25.hatenablog.com/entry/2016/11/21/222625 (Japanese)
;;;

(defpackage :cp/gabow-edmonds
  (:use :cl)
  (:export #:find-matching))
(in-package :cp/gabow-edmonds)

(defun find-matching (graph)
  "Computes a maximum matching in an undirected graph. -1 in the returned vector
expresses an unmatched vertex.

GRAPH := vector of adjacency lists

Example:
CL-USER> (gabow-edmonds #((1 0) (2 1) (0 1)))
-> #(2 -1 0)"
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (g (make-array (+ n 1) :element-type 'list :initial-element nil))
         (edge-count 0)
         (mate (make-array (+ n 1) :element-type 'fixnum :initial-element 0))
         (label (make-array (+ n 1) :element-type 'fixnum :initial-element -1))
         (first (make-array (+ n 1) :element-type 'fixnum :initial-element 0))
         (que (make-array n :element-type 'fixnum))
         (que-front 0)
         (que-end 0))
    (declare ((integer 0 #.most-positive-fixnum) edge-count que-front que-end))
    (dotimes (u n)
      (dolist (v (aref graph u))
        (declare ((integer 0 #.most-positive-fixnum) v))
        (when (< u v)
          (push (cons (+ u 1) (+ edge-count n 1))
                (aref g (+ v 1)))
          (push (cons (+ v 1) (+ edge-count n 1))
                (aref g (+ u 1)))
          (incf edge-count))))
    (let ((e1 (make-array edge-count :element-type 'fixnum))
          (e2 (make-array edge-count :element-type 'fixnum)))
      (let ((index 0))
        (dotimes (u n)
          (dolist (v (aref graph u))
            (declare ((integer 0 #.most-positive-fixnum) v))
            (when (< u v)
              (setf (aref e1 index) (+ u 1)
                    (aref e2 index) (+ v 1)
                    index (+ index 1))))))
      (labels ((enqueue (x)
                 (setf (aref que que-end) x)
                 (incf que-end))
               (dequeue ()
                 (prog1 (aref que que-front)
                   (incf que-front)))
               (eval-first (x)
                 (if (< (aref label (aref first x)) 0)
                     (aref first x)
                     (setf (aref first x)
                           (eval-first (aref first x)))))
               (queue-reinitialize ()
                 (setq que-front 0
                       que-end 0))
               (queue-empty-p () (= que-front que-end))
               (rematch (v w)
                 (let ((tmp (aref mate v)))
                   (setf (aref mate v) w)
                   (when (= (aref mate tmp) v)
                     (if (<= (aref label v) n)
                         (progn
                           (setf (aref mate tmp) (aref label v))
                           (rematch (aref label v) tmp))
                         (let ((x (aref e1 (- (aref label v) n 1)))
                               (y (aref e2 (- (aref label v) n 1))))
                           (rematch x y)
                           (rematch y x))))))
               (assign-label (x y num)
                 (declare ((integer 0 #.most-positive-fixnum) x y num))
                 (let ((r (eval-first x))
                       (s (eval-first y))
                       (join 0))
                   (unless (= r s)
                     (setf (aref label r) (- num)
                           (aref label s) (- num))
                     (loop
                       (unless (zerop s)
                         (rotatef r s))
                       (setq r (eval-first (aref label (aref mate r))))
                       (when (= (aref label r) (- num))
                         (setq join r)
                         (return))
                       (setf (aref label r) (- num)))
                     (let ((v (aref first x)))
                       (loop until (= v join)
                             do (enqueue v)
                                (setf (aref label v) num
                                      (aref first v) join
                                      v (aref first (aref label (aref mate v)))))
                       (setq v (aref first y))
                       (loop until (= v join)
                             do (enqueue v)
                                (setf (aref label v) num
                                      (aref first v) join
                                      v (aref first (aref label (aref mate v)))))))))
               (augment (u)
                 (setf (aref first u) 0
                       (aref label u) 0)
                 (enqueue u)
                 (loop until (queue-empty-p)
                       for x = (dequeue)
                       do (loop for (y . lab) in (aref g x)
                                do (cond ((and (zerop (aref mate y))
                                               (/= y u))
                                          (setf (aref mate y) x)
                                          (rematch x y)
                                          (return-from augment t))
                                         ((>= (aref label y) 0)
                                          (assign-label x y lab))
                                         ((< (aref label (aref mate y)) 0)
                                          (setf (aref label (aref mate y)) x
                                                (aref first (aref mate y)) y)
                                          (enqueue (aref mate y))))))
                 nil))
        (loop for i from 1 to n
              do (queue-reinitialize)
                 (when (zerop (aref mate i))
                   (when (augment i)
                     (fill label -1))))
        (dotimes (i (+ n 1))
          (if (> (aref mate i) 0)
              (decf (aref mate i))
              (setf (aref mate i) -1)))
        (subseq mate 1)))))
