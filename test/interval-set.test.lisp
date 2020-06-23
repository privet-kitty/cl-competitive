(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../interval-set.lisp"))

(use-package :test-util)

(defun to-interval-list (thing)
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
  (declare (simple-bit-vector vector))
  (loop for i from l below r
        do (setf (aref vector i) 1))
  vector)

(defun delete-interval (vector l r)
  (declare (simple-bit-vector vector))
  (loop for i from l below r
        do (setf (aref vector i) 0)))

(with-test (:name interval-set/random)
  (dotimes (_ 10000)
    (let ((vector (make-array 30 :element-type 'bit :initial-element 0))
          iset)
      (dotimes (_ 10)
        (let ((l (random 31))
              (r (random 31)))
          (when (> l r) (rotatef l r))
          (add-interval vector l r)
          (iset-push iset l r)
          (assert (equal (to-interval-list iset)
                         (to-interval-list vector))))))))
