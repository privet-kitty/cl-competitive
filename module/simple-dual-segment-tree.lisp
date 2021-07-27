(defpackage :cp/simple-dual-segment-tree
  (:use :cl)
  (:export #:define-simple-dual-segtree)
  (:documentation "Provides a restricted 1-dimensional dual segment tree, which
provides range update and point query. The meaning of `restricted' here is that
the operator to update range must be commutative."))
(in-package :cp/simple-dual-segment-tree)

(deftype index () '(mod #.(floor array-dimension-limit 2)))

(defmacro define-simple-dual-segtree (name &key (operator '#'+) (identity 0) element-type)
  "OPERATOR := commutative binary operator to update range
IDENTITY := identity element
ELEMENT-TYPE := type specifier for the given space

This macro defines five functions:

- <NAME>-REF: accessor
- <NAME>-UPDATE: function to update range
- MAKE-<NAME>: linear-time constructor"
  (let* ((fname-ref (intern (format nil "~A-REF" (symbol-name name))))
         (fname-update (intern (format nil "~A-UPDATE" (symbol-name name))))
         (fname-make (intern (format nil "MAKE-~A" (symbol-name name))))
         (fname-%make (intern (format nil "%MAKE-~A" (symbol-name name))))
         (fname-n (intern (format nil "%~A-N" (symbol-name name))))
         (fname-vector (intern (format nil "%~A-VECTOR" (symbol-name name))))
         (conc-name (intern (format nil "%~A-" (symbol-name name)))))
    `(progn
       (defstruct (,name (:constructor ,fname-%make
                             (vector &aux (n (ash (+ 1 (length vector)) -1))))
                         (:conc-name ,conc-name))
         (n nil :type index) ; length of original vector
         (vector nil :type (simple-array ,element-type (*))))
       (declaim (inline ,fname-make))
       (defun ,fname-make (size &key (initial-element ,identity) initial-contents)
         (declare (index size)
                  ((or null sequence) initial-contents))
         (let ((res (make-array (max 0 (- (* 2 size) 1))
                                :element-type ',element-type
                                :initial-element initial-element)))
           (fill res ,identity :end (max 0 (- size 1)))
           (when initial-contents
             (replace res initial-contents :start1 (max 0 (- size 1))))
           (,fname-%make res)))

       (defun ,fname-ref (,name index)
         "Returns the element at INDEX."
         (declare (index index))
         (let* ((vector (,fname-vector ,name))
                (i (+ index (- (,fname-n ,name) 1)))
                (res (aref vector i)))
           (declare (index i))
           (loop while (> i 0)
                 do (setq i (ash (- i 1) -1))
                    (setq res (funcall ,operator res (aref vector i))))
           res))

       (declaim (inline ,fname-update))
       (defun ,fname-update (,name operand left right)
         "Updates the given half-open range [LEFT, RIGHT) by OPERAND."
         (declare (index left right))
         (let* ((vector (,fname-vector ,name))
                (l (max 0 (+ left (,fname-n ,name) -1)))
                (r (max 0 (+ right (,fname-n ,name) -1))))
           (declare (index l r))
           (loop while (< l r)
                 when (evenp l)
                 do (setf (aref vector l) (funcall ,operator (aref vector l) operand))
                    (incf l)
                 when (evenp r)
                 do (decf r)
                    (setf (aref vector r) (funcall ,operator (aref vector r) operand))
                 do (setq l (ash (- l 1) -1)
                          r (ash (- r 1) -1))))))))

#+(or)
(define-simple-dual-segtree segtree
  :operator #'+
  :identity 0
  :element-type fixnum)
