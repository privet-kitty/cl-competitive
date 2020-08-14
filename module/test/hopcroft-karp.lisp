(defpackage :cp/test/hopcroft-karp
  (:use :cl :fiveam :cp/hopcroft-karp)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/hopcroft-karp)
(in-suite base-suite)

(defun coerce-to-bgraph2 (graph boundary)
  (let ((bgraph (make-bgraph boundary (- (length graph) boundary))))
    (dotimes (u boundary)
      (dolist (v (aref graph u))
        (bgraph-add-edge! bgraph u (- v boundary))))
    bgraph))

(test hopcroft-karp
  (let* ((graph (make-array 9
                            :element-type 'list
                            :initial-contents '((6) (5 6 7 8) (6) (6) (5) (1 4) (0 1 2 3) (1) (1))))
         (bgraph (coerce-to-bgraph2 graph 5))
         (count (bgraph-build-matching! bgraph))
         (matching1 (bgraph-matching1 bgraph))
         (matching2 (bgraph-matching2 bgraph)))
    (loop for i below (length matching1)
          do (is-true (or (= (aref matching1 i) -1)
                          (= i (aref matching2 (aref matching1 i))))))
    (loop for i below (length matching2)
          do (is-true (or (= (aref matching2 i) -1)
                          (= i (aref matching1 (aref matching2 i))))))
    (is (= count 3
           (count -1 matching1 :test-not #'=)
           (count -1 matching2 :test-not #'=))))
  ;; empty case
  (let ((bgraph (make-bgraph 0 0)))
    (is (zerop (bgraph-build-matching! bgraph)))
    (is (equalp #() (bgraph-matching1 bgraph)))
    (is (equalp #() (bgraph-matching2 bgraph)))))
