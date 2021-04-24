(defpackage :cp/2d-sliding-window
  (:use :cl)
  (:export #:make-2d-window-optimum)
  (:documentation "Provides sliding window minimum and maximum on 2D matrix."))
(in-package :cp/2d-sliding-window)

(declaim (ftype (function * (values (simple-array fixnum (* *)) &optional))
                make-2d-window-optimum)
         (inline make-2d-window-optimum))
(defun make-2d-window-optimum (mat height width &key (order #'<))
  "Returns a matrix which stores the minimum (or maximum, if ORDER is #'>) of
each rectangle area with size HEIGHT * WIDTH.

RESULT[i][j] := minimum [maximum] of sub-matrix between coordinates (max(0,
i-height+1), max(0, j-width+1)) and (i, j) both inclusive. Note that it
**doesn't** use half-open intervals."
  (declare ((array * (* *)) mat)
           ((integer 1 (#.array-dimension-limit)) height width))
  (destructuring-bind (h w) (array-dimensions mat)
    (declare ((mod #.array-dimension-limit) h w))
    (assert (and (<= height h) (<= width w)))
    (let ((deque (make-array (max h w) :element-type 'fixnum))
          (front 0)
          (end 0)
          (colopts (make-array (list h w) :element-type 'fixnum))
          (res (make-array (list h w) :element-type 'fixnum)))
      (declare ((mod #.array-dimension-limit) front end))
      (labels ((pop-front ()
                 (prog1 (aref deque front)
                   (incf front)))
               (pop-back ()
                 (decf end)
                 (aref deque end))
               (push-back (x)
                 (setf (aref deque end) x)
                 (incf end))
               (peek-back () (aref deque (- end 1)))
               (peek-front () (aref deque front))
               (clear () (setq front 0 end 0)))
        (dotimes (pivot-col w)
          (clear)
          (dotimes (row h)
            (let ((value (aref mat row pivot-col)))
              (loop until (or (= front end)
                              (funcall order (aref mat (peek-back) pivot-col) value))
                    do (pop-back))
              (loop until (or (= front end)
                              (< row (+ (peek-front) height)))
                    do (pop-front))
              (push-back row)
              (setf (aref colopts row pivot-col)
                    (aref mat (peek-front) pivot-col)))))
        (dotimes (pivot-row h)
          (clear)
          (dotimes (col w)
            (let ((value (aref colopts pivot-row col)))
              (loop until (or (= front end)
                              (funcall order (aref colopts pivot-row (peek-back)) value))
                    do (pop-back))
              (loop until (or (= front end)
                              (< col (+ (peek-front) width)))
                    do (pop-front))
              (push-back col)
              (setf (aref res pivot-row col)
                    (aref colopts pivot-row (peek-front))))))
        res))))
