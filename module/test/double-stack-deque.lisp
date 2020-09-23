(defpackage :cp/test/double-stack-deque
  (:use :cl :fiveam :cp/double-stack-deque)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/double-stack-deque)
(in-suite base-suite)

(test double-stack-deque/manual
  (let ((deque (make-deque '(5 6 7))))
    (is (= 7 (deque-pop-back deque)))
    (deque-push-front 4 deque)
    (deque-push-back 7 deque)
    (deque-push-back 8 deque)
    (is (= 4 (deque-pop-front deque)))
    (is (= 5 (deque-pop-front deque)))
    (is (= 6 (deque-pop-front deque)))
    (is (= 8 (deque-pop-back deque)))
    (is (= 7 (deque-pop-back deque)))
    (signals deque-empty-error (deque-pop-front deque))
    (signals deque-empty-error (deque-pop-back deque))))

(defun lastcar (list)
  (car (last list)))

(test double-stack-deque/random
  (let ((state (sb-ext:seed-random-state 0))
        (deq (make-deque (list 1 2 3 4 5)))
        (list (list 1 2 3 4 5)))
    (dotimes (_ 100)
      (finishes
        (dotimes (_ 500)
          (ecase (random 7 state)
            ;; peek front
            (0 (if list
                   (assert (= (car list) (deque-peek-front deq)))
                   (deque-empty-p deq)))
            ;; peek back
            (1 (if list
                   (assert (= (lastcar list) (deque-peek-back deq)))
                   (deque-empty-p deq)))
            ;; push front
            (2 (let ((value (random 1000 state)))
                 (deque-push-front value deq)
                 (push value list)))
            ;; push back
            (3 (let ((value (random 1000 state)))
                 (deque-push-back value deq)
                 (setq list (append list (list value)))))
            ;; pop front
            (4 (if list
                   (assert (= (pop list) (deque-pop-front deq)))
                   (deque-empty-p deq)))
            ;; pop back
            (5 (if list
                   (progn
                     (assert (= (lastcar list) (deque-pop-back deq)))
                     (setq list (subseq list 0 (- (length list) 1))))
                   (deque-empty-p deq)))
            ;; empty-p
            (6 (assert (if list
                           (not (deque-empty-p deq))
                           (deque-empty-p deq))))))))))
