(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../hopcroft-karp.lisp"))

(use-package :test-util)

(defun coerce-to-bgraph (graph boundary)
  (let ((bgraph (make-bgraph boundary (- (length graph) boundary))))
    (dotimes (u boundary)
      (dolist (v (aref graph u))
        (bgraph-add-edge! bgraph u (- v boundary))))
    bgraph))

(with-test (:name hopcroft-karp)
  (let* ((graph (make-array 9
                            :element-type 'list
                            :initial-contents '((6) (5 6 7 8) (6) (6) (5) (1 4) (0 1 2 3) (1) (1))))
         (bgraph (coerce-to-bgraph graph 5))
         (count (bgraph-build-matching! bgraph))
         (matching1 (bgraph-matching1 bgraph))
         (matching2 (bgraph-matching2 bgraph)))
    (loop for i below (length matching1)
          do (assert (or (= (aref matching1 i) -1)
                         (= i (aref matching2 (aref matching1 i))))))
    (loop for i below (length matching2)
          do (assert (or (= (aref matching2 i) -1)
                         (= i (aref matching1 (aref matching2 i))))))
    (assert (= count 3
               (count -1 matching1 :test-not #'=)
               (count -1 matching2 :test-not #'=))))
  ;; empty case
  (let ((bgraph (make-bgraph 0 0)))
    (assert (zerop (bgraph-build-matching! bgraph)))
    (assert (equalp #() (bgraph-matching1 bgraph)))
    (assert (equalp #() (bgraph-matching2 bgraph)))))
