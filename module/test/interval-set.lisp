(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../interval-set.lisp"))

(use-package :test-util)

(defun to-interval-list (thing)
  (declare (optimize (speed 3)))
  (etypecase thing
    (simple-bit-vector
     (let* ((n (length thing))
            (index1 (or (position 1 thing) n)))
       (loop while (< index1 n)
             for index0 = (or (position 0 thing :start index1) n)
             collect (cons index1 index0)
             do (setq index1 (or (position 1 thing :start index0) n)))))
    ((or null interval-set)
     (let (res)
       (iset-map (lambda (l r) (push (cons l r) res)) thing)
       (nreverse res)))))

(defun add-interval (vector l r)
  (declare (optimize (speed 3))
           (simple-bit-vector vector))
  (loop for i from l below r
        do (setf (aref vector i) 1))
  vector)

(defun delete-interval (vector l r)
  (declare (optimize (speed 3))
           (simple-bit-vector vector))
  (loop for i from l below r
        do (setf (aref vector i) 0)))

(defparameter *state* (seed-random-state 123))

(with-test (:name interval-set/random)
  (loop for len from 1 to 500
        for vector = (make-array len :element-type 'bit :initial-element 0)
        for iset = nil
        do (dotimes (_ 500)
             (let ((l (random (+ 1 len) *state*))
                   (r (random (+ 1 len) *state*)))
               (when (> l r) (rotatef l r))
               (if (zerop (random 2))
                   (progn
                     (add-interval vector l r)
                     (iset-push iset l r))
                   (progn
                     (delete-interval vector l r)
                     (iset-pop iset l r)))
               (assert (equal (to-interval-list iset)
                              (to-interval-list vector)))))))
