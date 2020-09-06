(defpackage :cp/test/deque
  (:use :cl :fiveam :cp/deque)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/deque)
(in-suite base-suite)

(define-deque deque :element-type (unsigned-byte 16))

(test deque/manual
  (declare (notinline deque-empty-p deque-ref deque-push-front deque-pop-front deque-push-back deque-pop-back deque-reinitialize))
  (let ((deque (make-deque 4)))
    ;; ()
    (is (deque-empty-p deque))
    (signals deque-empty-error (deque-pop-back deque))
    (signals deque-empty-error (deque-pop-front deque))
    (signals deque-empty-error (deque-peek-back deque))
    (signals deque-empty-error (deque-peek-front deque))
    (signals deque-invalid-index-error (deque-ref deque 0))
    (deque-push-front 1 deque)
    (is (= 1 (deque-ref deque 0)))
    ;; (1)
    (is (not (deque-empty-p deque)))
    (is (= 1 (deque-peek-front deque)))
    (is (= 1 (deque-peek-back deque)))
    (signals deque-invalid-index-error (deque-ref deque 1))
    (deque-push-back 2 deque)
    ;; (1 2)
    (is (= 1 (deque-ref deque 0)))
    (is (= 2 (deque-ref deque 1)))
    (deque-push-back 3 deque)
    ;; (1 2 3)
    (is (= 1 (deque-ref deque 0)))
    (is (= 2 (deque-ref deque 1)))
    (is (= 3 (deque-ref deque 2)))
    (signals deque-invalid-index-error (deque-ref deque 3))
    (deque-push-front 4 deque)
    ;; (4 1 2 3)
    (is (= 4 (deque-ref deque 0)))
    (is (= 1 (deque-ref deque 1)))
    (is (= 2 (deque-ref deque 2)))
    (is (= 3 (deque-ref deque 3)))
    (signals deque-invalid-index-error (deque-ref deque 4))
    (is (= 4 (deque-pop-front deque)))
    ;; (1 2 3)
    (is (= 1 (deque-ref deque 0)))
    (is (= 2 (deque-ref deque 1)))
    (is (= 3 (deque-ref deque 2)))
    (is (= 1 (deque-pop-front deque)))
    ;; (2 3)
    (is (= 2 (deque-count deque)))
    (is (= 2 (deque-peek-front deque)))
    (is (= 3 (deque-peek-back deque)))
    (is (= 2 (deque-pop-front deque)))
    ;; (3)
    (deque-push-front 5 deque)
    ;; (5 3)
    (is (= 5 (deque-ref deque 0)))
    (is (not (deque-empty-p deque)))
    (is (= 3 (deque-pop-back deque)))
    ;; (5)
    (is (= 5 (deque-pop-back deque)))
    ;; ()
    (signals deque-empty-error (deque-pop-back deque))
    (is (deque-empty-p deque))
    (deque-push-back 10 deque)
    ;; (10)
    (deque-reinitialize deque)
    (is (deque-empty-p deque)))
  
  ;; zero length
  (let ((deque (make-deque 0)))
    (is (deque-empty-p deque))
    (signals deque-empty-error (deque-pop-back deque))
    (signals deque-empty-error (deque-pop-front deque))
    (signals deque-invalid-index-error (deque-ref deque 0))))

(defun lastcar (list)
  (car (last list)))

(test deque/random
  (let ((state (sb-ext:seed-random-state 0))
        (deq (make-deque 0))
        list)
    (dotimes (_ 100)
      (finishes
        (dotimes (_ 500)
          (ecase (random 10 state)
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
            ;; ref
            (6 (if list
                   (let ((index (random (deque-count deq) state)))
                     (assert (= (nth index list) (deque-ref deq index))))
                   (deque-empty-p deq)))
            ;; set
            (7 (if list
                   (let ((index (random (deque-count deq) state))
                         (value (random 1000 state)))
                     (setf (deque-ref deq index) value
                           (nth index list) value))
                   (deque-empty-p deq)))
            ;; empty-p
            (8 (assert (if list
                           (not (deque-empty-p deq))
                           (deque-empty-p deq))))
            ;; reinitialize
            (9
             (deque-reinitialize deq)
             (setq list nil))))))))
