(defpackage :cp/test/interval-map
  (:use :cl :fiveam :cp/interval-map :cp/run-range)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/interval-map)
(in-suite base-suite)

(defun to-interval-list (thing)
  (declare (optimize (speed 3)))
  (etypecase thing
    (simple-vector
     (let* (res)
       (map-run-range (lambda (val l r)
                        (when val
                          (push (list* l r val) res)))
                      thing
                      :test #'eq)
       (nreverse res)))
    ((or null interval-map)
     (let (res)
       (imap-map (lambda (l r val) (push (list* l r val) res)) thing)
       (nreverse res)))))

(defun add-interval (vector l r val)
  (declare (optimize (speed 3))
           (simple-vector vector))
  (loop for i from l below r
        do (setf (aref vector i) val))
  vector)

(defun delete-interval (vector l r)
  (declare (optimize (speed 3))
           (simple-vector vector))
  (loop for i from l below r
        do (setf (aref vector i) nil)))

(test interval-map/hand
  (let ((imap (imap-insert nil 3 10 #\a)))
    (is (equalp '((3 10 . #\a)) (to-interval-list imap)))
    (setq imap (imap-insert imap 4 6 #\b))
    (is (equalp '((3 4 . #\a) (4 6 . #\b) (6 10 . #\a)) (to-interval-list imap)))
    (setq imap (imap-insert imap -1 0 #\a))
    (is (equalp '((-1 0 . #\a) (3 4 . #\a) (4 6 . #\b) (6 10 . #\a)) (to-interval-list imap)))
    (setq imap (imap-insert imap -2 5 #\c))
    (is (equalp '((-2 5 . #\c) (5 6 . #\b) (6 10 . #\a)) (to-interval-list imap)))
    (setq imap (imap-insert imap -10 10 #\a))
    (is (equalp '((-10 10 . #\a)) (to-interval-list imap)))
    (is (equalp '(nil nil nil) (multiple-value-list (imap-get imap 10))))
    (is (equalp '(#\a -10 10) (multiple-value-list (imap-get imap 9))))
    (multiple-value-bind (l r) (imap-split imap 8)
      (is (equalp '((-10 8 . #\a)) (to-interval-list l)))
      (is (equalp '((8 10 . #\a)) (to-interval-list r)))
      (setq imap (imap-concat l r)))
    (is (equalp '((-10 8 . #\a) (8 10 . #\a)) (to-interval-list imap)))))

(test interval-map/random
  (let ((*test-dribble* nil)
        (state (sb-ext:seed-random-state 123)))
    (loop for len from 1 to 200
          for vector = (make-array len :element-type t :initial-element nil)
          for imap = nil
          do (dotimes (i 200)
               (let ((l (random (+ 1 len) state))
                     (r (random (+ 1 len) state)))
                 (when (> l r) (rotatef l r))
                 (add-interval vector l r i)
                 (setq imap (imap-insert imap l r i))
                 (is (equal (to-interval-list imap)
                            (to-interval-list vector))))))))
