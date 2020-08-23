;;;
;;; Offline dynamic connectivity
;;;

(defpackage :cp/offline-dynamic-connectivity
  (:use :cl :cp/undoable-disjoint-set)
  (:export #:dynamic-connectivity #:make-dynamic-connectivity
           #:dycon-insert #:dycon-delete #:dycon-build #:dycon-map
           #:dycon-num-components #:dycon-disjoint-set))
(in-package :cp/offline-dynamic-connectivity)

;; NOTE: not tested
;; NOTE: MAX-TIME must be positive.

(defstruct (dynamic-connectivity
            (:constructor make-dynamic-connectivity
                (size max-time
                 &aux (segtree (make-array (- (* 2 max-time) 1) :element-type 'list :initial-element nil))
                      (counter (make-hash-table :test #'equal))
                      (appearance (make-hash-table :test #'equal))
                      (events (make-array 0 :element-type 'list :fill-pointer 0))
                      (disjoint-set (make-undoable-disjoint-set size max-time))
                      (num-components size)))
            (:copier nil)
            (:predicate nil)
            (:conc-name dycon-))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (max-time 0 :type (integer 1 #.most-positive-fixnum))
  (last-time 0 :type (integer 0 #.most-positive-fixnum))
  (segtree nil :type (simple-array list (*)))
  (counter nil :type hash-table)
  (appearance nil :type hash-table)
  ;; (appear-time disappear-time vertex1 . vertex2)  
  (events nil :type (array list (*)))
  ;; disjoint set that holds connectivity of graph
  (disjoint-set nil :type undoable-disjoint-set)
  ;; number of connected components
  (num-components 0 :type (integer 0 #.most-positive-fixnum)))

(defun dycon-insert (dycon u v time)
  "Inserts an edge {u, v} at TIME."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) u v time))
  (symbol-macrolet ((last-time (dycon-last-time dycon))
                    (counter (dycon-counter dycon))
                    (appearance (dycon-appearance dycon)))
    (assert (>= time last-time))
    (setf last-time time)
    (when (> u v) (rotatef u v))
    (let* ((edge (cons u v))
           (count (gethash edge counter)))
      (declare ((or null (integer 0 #.most-positive-fixnum)) count))
      (if count
          (setf (gethash edge counter) (+ count 1))
          (setf (gethash edge appearance) time
                (gethash edge counter) 1)))))

(defun dycon-delete (dycon u v time)
  "Deletes an edge {u, v} at TIME."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) u v time))
  (symbol-macrolet ((last-time (dycon-last-time dycon))
                    (counter (dycon-counter dycon))
                    (appearance (dycon-appearance dycon))
                    (events (dycon-events dycon)))
    (assert (>= time last-time))
    (setf last-time time)
    (when (> u v) (rotatef u v))
    (let* ((edge (cons u v))
           (count (gethash edge counter)))
      (declare ((or null (integer 0 #.most-positive-fixnum)) count))
      (assert count () "Attempted to delete non-existent edge (~W . ~W) at time ~W"
              u v time)
      (if (= count 1)
          (let ((appear-time (gethash edge appearance)))
            (remhash edge counter)
            (unless (eql appear-time time)
              (vector-push-extend (list* appear-time time edge) events)))
          (setf (gethash edge counter) (- count 1))))))

(defun %dycon-update (dycon edge l r)
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) l r))
  (let ((segtree (dycon-segtree dycon))
        (max-time (dycon-max-time dycon)))
    (incf l (- max-time 1))
    (incf r (- max-time 1))
    (loop while (< l r)
          when (evenp l)
          do (push edge (aref segtree l))
             (incf l)
          when (evenp r)
          do (decf r)
             (push edge (aref segtree r))
          do (setq l (ash (- l 1) -1)
                   r (ash (- r 1) -1)))))

(defun dycon-build (dycon)
  (declare (optimize (speed 3)))
  (let ((counter (dycon-counter dycon))
        (events (dycon-events dycon))
        (appearance (dycon-appearance dycon))
        (max-time (dycon-max-time dycon)))
    (maphash (lambda (edge count)
               (declare (ignore count))
               (let ((appear-time (gethash edge appearance)))
                 (vector-push-extend (list* appear-time max-time edge) events)))
             counter)
    (loop for (appear-time disappear-time u . v) across events
          do (%dycon-update dycon (cons u v) appear-time disappear-time))
    dycon))

(defun dycon-map (dycon function)
  "FUCTION takes an argument, time: When FUNCTION is called, NUM-COMPONENTS and
DISJOINT-SET become those of the time. Be sure to call DYCON-BUILD beforehand."
  (declare (optimize (speed 3))
           (function function))
  (symbol-macrolet ((comp (dycon-num-components dycon)))
    (let* ((disjoint-set (dycon-disjoint-set dycon))
           (segtree (dycon-segtree dycon))
           (max-time (dycon-max-time dycon)))
      (labels ((recur (i)
                 (declare ((integer 0 #.most-positive-fixnum) i))
                 (let ((comp-delta 0))
                   (declare ((integer 0 #.most-positive-fixnum) comp-delta))
                   (loop for (u . v) in (aref segtree i)
                         when (uds-unite! disjoint-set u v)
                         do (incf comp-delta))
                   (decf comp comp-delta)
                   (if (< i (- max-time 1))
                       (progn
                         (recur (+ 1 (* 2 i)))
                         (recur (+ 2 (* 2 i))))
                       (funcall function (- i (- max-time 1))))
                   (incf comp comp-delta)
                   (loop for edge in (aref segtree i)
                         do (uds-undo! disjoint-set)))))
        (recur 0)))))
