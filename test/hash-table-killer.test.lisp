(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../hash-table-killer.lisp"))

(use-package :test-util)

(defun hash-collide-p (size max timeout)
  (let ((table (make-hash-table :size size :test #'eq))
        (keys (make-killer-sequence2 size max)))
    (sb-int:with-progressive-timeout (get-remaining-time :seconds timeout)
      (dolist (key keys)
        (when (zerop (get-remaining-time))
          (return-from hash-collide-p t))
        (setf (gethash key table) t)))
    nil))

(with-test (:name hash-table-killer)
  (assert (hash-collide-p 100000 (expt 10 18) 0.3))
  (assert (not (hash-collide-p 100000 (expt 10 10) 0.3))))
