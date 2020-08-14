(defpackage :cp/dotimes-unroll
  (:use :cl)
  (:export #:dotimes-unroll #:dotimes-unroll-all))
(in-package :cp/dotimes-unroll)

;; FIXME: currently not enclosed with (BLOCK NIL ...).
(defmacro dotimes-unroll ((var count size &optional result) &body body)
  "DOTIMES macro with loop-unrolling by SIZE."
  (let ((whole (gensym))
        (sup (gensym))
        (outer-index (gensym)))
    (check-type var symbol)
    (check-type size (integer 1 #.most-positive-fixnum))
    (if (integerp count)
        ;; We can unroll the loop more concretely if COUNT is constant
        (multiple-value-bind (quot rem) (floor count size)
          (assert (>= quot 0))
          `(let ((,var 0))
             (declare ((integer 0 #.most-positive-fixnum) ,var))
             ,(unless (zerop quot)
                `(dotimes (,outer-index ,quot)
                   ,@(loop repeat size
                           collect `(locally ,@body (setq ,var (+ ,var 1))))))
             ,@(loop for delta below rem
                     collect `(locally ,@body (setq ,var ,(+ 1 (* quot size) delta))))
             ,result))
        `(block ,whole
           (let ((,sup ,count)
                 (,var 0))
             (declare ((integer 0 #.most-positive-fixnum) ,var ,sup))
             (loop
               (when (> ,var (- ,sup ,size))
                 (do ((,var ,var (1+ ,var)))
                     ((>= ,var ,sup)
                      (return-from ,whole ,result))
                   ,@body))
               ,@(loop for i from 0 below size
                       collect `(locally ,@body (setq ,var (+ ,var 1))))))))))

(defmacro dotimes-unroll-all ((var count &optional result) &body body)
  "Unrolls all loops. COUNT must be an integer literal."
  (check-type count (integer 0))
  `(block nil
     ,@(loop for i from 0 below count
             collect `(let ((,var ,i)) ,@body))
     ,result))
