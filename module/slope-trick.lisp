(defpackage :cp/slope-trick
  (:use :cl :cp/binary-heap)
  (:export #:make-slope-trick #:strick-add+ #:strick-add- #:strick-add
           #:strick-min #:strick-argmin #:strick-shift #:strick-left-cum #:strick-right-cum)
  (:documentation
   "Reference:
https://maspypy.com/slope-trick-1-%e8%a7%a3%e8%aa%ac%e7%b7%a8"))
(in-package :cp/slope-trick)

;; TODO: add test

(define-binary-heap heap<
  :order #'<
  :element-type fixnum)

(define-binary-heap heap>
  :order #'>
  :element-type fixnum)

(defstruct (slope-trick (:constructor make-slope-trick
                            (lsize
                             &optional (rsize lsize) 
                             &aux (lheap (make-heap> lsize))
                                  (rheap (make-heap< rsize))))
                        (:conc-name %strick-)
                        (:copier nil)
                        (:predicate nil))
  (min 0 :type fixnum)
  (lheap nil :type heap>)
  (rheap nil :type heap<)
  ;; pop: +offset; push: -offset
  (loffset 0 :type fixnum)
  (roffset 0 :type fixnum))

(declaim (inline strick-min))
(defun strick-min (slope-trick)
  (%strick-min slope-trick))
(declaim (inline  (setf strick-min)))
(defun (setf strick-min) (new-value slope-trick)
  (setf (%strick-min slope-trick) new-value))

(declaim (ftype (function * (values (or null fixnum) (or null fixnum) &optional))
                strick-argmin))
(defun strick-argmin (slope-trick)
  "Returns two values: the left end and the right end of the closed interval on
which f takes the minimum. Positive and negative infinities are represented by
NIL."
  (let ((lheap (%strick-lheap slope-trick))
        (rheap (%strick-rheap slope-trick))
        (loffset (%strick-loffset slope-trick))
        (roffset (%strick-roffset slope-trick)))
    (values (unless (heap>-empty-p lheap)
              (+ loffset (heap>-peek lheap)))
            (unless (heap<-empty-p rheap)
              (+ roffset (heap<-peek rheap))))))

(defun strick-add+ (slope-trick a)
  "Adds x |-> max(0, x-a) to f."
  (declare (fixnum a))
  (symbol-macrolet ((min (%strick-min slope-trick))
                    (lheap (%strick-lheap slope-trick))
                    (rheap (%strick-rheap slope-trick))
                    (loffset (%strick-loffset slope-trick))
                    (roffset (%strick-roffset slope-trick)))
    (unless (heap>-empty-p lheap)
      (incf min (max 0 (- (+ (heap>-peek lheap) loffset) a))))
    (heap>-push (- a loffset) lheap)
    (heap<-push (- (+ loffset (heap>-pop lheap)) roffset) rheap)
    slope-trick))

(defun strick-add- (slope-trick a)
  "Adds x |-> max(0, a-x) to f."
  (declare (fixnum a))
  (symbol-macrolet ((min (%strick-min slope-trick))
                    (lheap (%strick-lheap slope-trick))
                    (rheap (%strick-rheap slope-trick))
                    (loffset (%strick-loffset slope-trick))
                    (roffset (%strick-roffset slope-trick)))
    (unless (heap<-empty-p rheap)
      (incf min (max 0 (- a (+ (heap<-peek rheap) roffset)))))
    (heap<-push (- a roffset) rheap)
    (heap>-push (- (+ roffset (heap<-pop rheap)) loffset) lheap)
    slope-trick))

(defun strick-add (slope-trick a)
  "Adds x |-> abs(x-a) to f."
  (declare (fixnum a))
  (strick-add+ slope-trick a)
  (strick-add- slope-trick a))

(defun strick-left-cum (slope-trick)
  "Replaces f to g such that g(x) := min_{t <= x} f(t)."
  (heap<-clear (%strick-rheap slope-trick))
  (setf (%strick-roffset slope-trick) 0)
  slope-trick)

(defun strick-right-cum (slope-trick)
  "Replaces f to g such that g(x) := min_{x <= t} f(t)"
  (heap>-clear (%strick-lheap slope-trick))
  (setf (%strick-loffset slope-trick) 0)
  slope-trick)

(defun strick-shift (slope-trick ldelta &optional rdelta)
  "Replaces f to g such that g(x) := min_{x-r <= t <= x-l}f(x)."
  (declare (optimize (speed 3))
           (fixnum ldelta)
           ((or null fixnum) rdelta))
  (let ((rdelta (or rdelta ldelta)))
    (assert (<= ldelta rdelta))
    (incf (%strick-loffset slope-trick) ldelta)
    (incf (%strick-roffset slope-trick) rdelta)
    slope-trick))
