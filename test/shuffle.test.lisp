(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../shuffle.lisp"))

(use-package :test-util)

(defun set-equal (seq1 seq2)
  (let ((table1 (make-hash-table))
        (table2 (make-hash-table)))
    (loop for x being each element of seq1
          do (setf (gethash x table1) t))
    (loop for x being each element of seq2
          do (setf (gethash x table2) t))
    (and (loop for x being each element of seq1
               always (gethash x table2))
         (loop for x being each element of seq2
               always (gethash x table1)))))

(with-test (:name shuffle!)
  (let ((seq #(0 1 2 3 4)))
    (dotimes (_ 100)
      (set-equal seq (shuffle! (copy-seq seq)))))
  (set-equal (vector) (shuffle! (vector))))
