(defpackage :cp/slope-trick
  (:use :cl :cp/binary-heap)
  (:export #:make-slope-trick #:strick-add+ #:strick-add- #:strick-add #:strick-shift
           #:strick-min #:strick-argmin #:strick-left-cum #:strick-right-cum #:strick-merge
           #:strick-calc)
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
  (declare (optimize (speed 3))
           (fixnum a))
  (symbol-macrolet ((min (%strick-min slope-trick))
                    (lheap (%strick-lheap slope-trick))
                    (rheap (%strick-rheap slope-trick))
                    (loffset (%strick-loffset slope-trick))
                    (roffset (%strick-roffset slope-trick)))
    (unless (heap>-empty-p lheap)
      (incf min (max 0 (the fixnum (- (the fixnum (+ (heap>-peek lheap) loffset)) a)))))
    (heap>-push (the fixnum (- a loffset)) lheap)
    (heap<-push (the fixnum (- (the fixnum (+ loffset (heap>-pop lheap))) roffset)) rheap)
    slope-trick))

(defun strick-add- (slope-trick a)
  "Adds x |-> max(0, a-x) to f."
  (declare (optimize (speed 3))
           (fixnum a))
  (symbol-macrolet ((min (%strick-min slope-trick))
                    (lheap (%strick-lheap slope-trick))
                    (rheap (%strick-rheap slope-trick))
                    (loffset (%strick-loffset slope-trick))
                    (roffset (%strick-roffset slope-trick)))
    (unless (heap<-empty-p rheap)
      (incf min (max 0 (the fixnum (- a (the fixnum (+ (heap<-peek rheap) roffset)))))))
    (heap<-push (the fixnum (- a roffset)) rheap)
    (heap>-push (the fixnum (- (the fixnum (+ roffset (heap<-pop rheap))) loffset)) lheap)
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

(declaim (ftype (function * (values (mod #.array-dimension-limit) &optional))
                strick-size))
(defun strick-size (slope-trick)
  (+ (heap>-count (%strick-lheap slope-trick))
     (heap<-count (%strick-rheap slope-trick))))

(defun strick-merge (slope-trick1 slope-trick2)
  "Merges two SLOPE-TRICKs. You cannot use a side effect. Use a returned
value."
  (declare (optimize (speed 3)))
  (when (< (strick-size slope-trick1) (strick-size slope-trick2))
    (rotatef slope-trick1 slope-trick2))
  (let ((loffset2 (%strick-loffset slope-trick2)))
    (heap>-map (lambda (a)
                 (strick-add- slope-trick1 (the fixnum (+ a loffset2))))
               (%strick-lheap slope-trick2)))
  (let ((roffset2 (%strick-roffset slope-trick2)))
    (heap<-map (lambda (a)
                 (strick-add+ slope-trick1 (the fixnum (+ a roffset2))))
               (%strick-rheap slope-trick2)))
  (incf (%strick-min slope-trick1) (%strick-min slope-trick2))
  slope-trick1)

(defun strick-calc (slope-trick x)
  (declare (optimize (speed 3))
           (fixnum x))
  (multiple-value-bind (l r) (strick-argmin slope-trick)
    (let ((res (strick-min slope-trick))
          (slope 0)
          (lheap (%strick-lheap slope-trick))
          (loffset (%strick-loffset slope-trick))
          (rheap (%strick-rheap slope-trick))
          (roffset (%strick-roffset slope-trick)))
      (declare (fixnum slope))
      (cond ((<= l x r) res)
            ((< r x)
             (let ((prev r))
               (declare (fixnum prev))
               (loop (when (heap<-empty-p rheap)
                       (return))
                     (let ((a (+ roffset (heap<-pop rheap))))
                       (declare (fixnum a))
                       (when (< x a)
                         (return))
                       (incf res (* slope (- a prev)))
                       (setq prev a)
                       (incf slope)))
               (incf res (* slope (- x prev)))))
            ((< x l)
             (let ((prev l))
               (declare (fixnum prev))
               (loop (when (heap>-empty-p lheap)
                       (return))
                     (let ((a (+ loffset (heap>-pop lheap))))
                       (declare (fixnum a))
                       (when (< a x)
                         (return))
                       (incf res (* slope (- prev a)))
                       (setq prev a)
                       (decf slope)))
               (incf res (* slope (- prev x))))))
      res)))
