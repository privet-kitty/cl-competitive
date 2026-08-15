(defpackage :cp/test/multi-slope-trick-rollback
  (:use :cl :fiveam :cp/multi-slope-trick :cp/multi-slope-trick-rollback)
  (:import-from :cp/multi-slope-trick
                #:%mstrick-segments #:%mstrick-min-slope #:%mstrick-anchor-value
                #:%mstrick-dom-min
                #:%node-width #:%node-slope-gap #:%node-width-sum
                #:%node-slope-gap-sum #:%node-bregman #:%node-priority
                #:%node-left #:%node-right)
  (:import-from :cp/test/multi-slope-trick
                #:validate #:random-start #:check-against
                #:pl-inf-conv #:pl-pointwise-add #:pl-translate)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multi-slope-trick-rollback)
(in-suite base-suite)

(defun snap-tree (node)
  "Deep-copies the treap into nested lists -- every field, aggregates and
priorities included -- for exact restoration checks after a rollback."
  (and node
       (list (%node-width node) (%node-slope-gap node) (%node-priority node)
             (%node-width-sum node) (%node-slope-gap-sum node)
             (%node-bregman node)
             (snap-tree (%node-left node)) (snap-tree (%node-right node)))))

(defun snap-state (f)
  (list (%mstrick-dom-min f) (%mstrick-anchor-value f)
        (%mstrick-min-slope f) (snap-tree (%mstrick-segments f))))

(test multi-slope-trick-rollback/random-inf-conv
  ;; The journaled union merges exactly like the plain one, and the rollback
  ;; restores both operands exactly -- shape, priorities, and every field.
  (let ((*random-state* (sb-ext:seed-random-state 13))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 3000)
        (multiple-value-bind (f pl) (random-start 6)
          (multiple-value-bind (g pl-g) (random-start 6)
            (let ((state-f (snap-state f))
                  (state-g (snap-state g))
                  (token (mstrick-inf-conv-with-rollback f g)))
              (pl-inf-conv pl pl-g)
              (check-against f pl)
              (let ((other (mstrick-inf-conv-rollback f token)))
                (unless (eq other g)
                  (error "rollback did not return the consumed operand"))
                (validate f)
                (validate g)
                (unless (and (equal state-f (snap-state f))
                             (equal state-g (snap-state g)))
                  (error "inf-conv rollback did not restore the operands"))))))))))

(test multi-slope-trick-rollback/random-pointwise-add
  (let ((*random-state* (sb-ext:seed-random-state 14))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 3000)
        (multiple-value-bind (f pl) (random-start 6)
          (multiple-value-bind (g pl-g) (random-start 6)
            ;; Slide G onto F's domain so the windows overlap.
            (let ((delta (- (+ (mstrick-dom-min f)
                               (random (1+ (- (mstrick-dom-max f)
                                              (mstrick-dom-min f)))))
                            (mstrick-dom-min g))))
              (mstrick-translate g delta)
              (pl-translate pl-g delta))
            (let ((state-f (snap-state f))
                  (state-g (snap-state g))
                  (token (mstrick-pointwise-add-with-rollback f g)))
              (pl-pointwise-add pl pl-g)
              (check-against f pl)
              (let ((other (mstrick-pointwise-add-rollback f token)))
                (unless (eq other g)
                  (error "rollback did not return the consumed operand"))
                (validate f)
                (validate g)
                (unless (and (equal state-f (snap-state f))
                             (equal state-g (snap-state g)))
                  (error "pointwise-add rollback did not restore the operands"))))))))))

(test multi-slope-trick-rollback/random-nested
  ;; A stack of journaled merges, restricts, and translates unwound in LIFO
  ;; order -- the usage pattern of a divide-and-conquer consumer -- restores
  ;; every intermediate state exactly.
  (let ((*random-state* (sb-ext:seed-random-state 15))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 1500)
        (multiple-value-bind (f pl) (random-start 8)
          (declare (ignore pl))
          (let ((states (list (snap-state f)))
                (stack nil))
            (dotimes (_ 5)
              (multiple-value-bind (g pl-g) (random-start 5)
                (declare (ignore pl-g))
                (ecase (random 4)
                  (0 (push (list :inf-conv g (snap-state g)
                                 (mstrick-inf-conv-with-rollback f g))
                           stack))
                  (1 (mstrick-translate
                      g (- (+ (mstrick-dom-min f)
                              (random (1+ (- (mstrick-dom-max f)
                                             (mstrick-dom-min f)))))
                           (mstrick-dom-min g)))
                     (push (list :pointwise-add g (snap-state g)
                                 (mstrick-pointwise-add-with-rollback f g))
                           stack))
                  (2 (let ((c (+ (mstrick-dom-min f)
                                 (random (1+ (- (mstrick-dom-max f)
                                                (mstrick-dom-min f)))))))
                       (push (list :restrict nil nil
                                   (mstrick-restrict-dom-min f c))
                             stack)))
                  (3 (let ((delta (- (random 9) 4)))
                       (mstrick-translate f delta)
                       (push (list :translate nil nil delta) stack))))
                (push (snap-state f) states)))
            (pop states)
            (dolist (entry stack)
              (destructuring-bind (kind g state-g token) entry
                (ecase kind
                  (:inf-conv
                   (let ((other (mstrick-inf-conv-rollback f token)))
                     (unless (and (eq other g) (equal state-g (snap-state g)))
                       (error "nested inf-conv rollback failed"))))
                  (:pointwise-add
                   (let ((other (mstrick-pointwise-add-rollback f token)))
                     (unless (and (eq other g) (equal state-g (snap-state g)))
                       (error "nested pointwise-add rollback failed"))))
                  (:restrict (mstrick-restrict-dom-min-rollback f token))
                  (:translate (mstrick-translate f (- token))))
                (unless (equal (pop states) (snap-state f))
                  (error "nested rollback did not restore the function"))))))))))
