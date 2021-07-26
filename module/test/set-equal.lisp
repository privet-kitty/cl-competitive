(defpackage :cp/test/set-equal
  (:use :cl :fiveam :cp/set-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/set-equal)
(in-suite base-suite)

(test set-equal
  (let ((*test-dribble* nil))
    (dotimes (len 10)
      (dotimes (_ 100)
        (let ((list1 (loop repeat (+ len (random 3)) collect (random 5)))
              (list2 (loop repeat (+ len (random 3)) collect (random 5))))
          (is (eql (set-equal list1 list2)
                   (null (set-exclusive-or list1 list2)))))))))

(defun multiset-equal2 (list1 list2)
  (equal (sort (copy-list list1) #'<)
         (sort (copy-list list2) #'<)))

(test multiset-equal
  (let ((*test-dribble* nil))
    (dotimes (len 6)
      (dotimes (_ 500)
        (let ((list1 (loop repeat (+ len (random 2)) collect (random 5)))
              (list2 (loop repeat (+ len (random 2)) collect (random 5))))
          (is (eql (multiset-equal list1 list2)
                   (multiset-equal2 list1 list2))))))))
