(defpackage :cp/2d-bit
  (:use :cl)
  (:export #:define-2d-bitree)
  (:documentation "Provides 2-dimensional binary indexed tree."))
(in-package :cp/2d-bit)

;; TODO: binary search
(defmacro define-2d-bitree (name &key (operator '#'+) (identity 0) sum-type)
  "OPERATOR := binary operator (comprising a commutative monoid)
IDENTITY := object (identity element of the monoid)
SUM-TYPE := nil | type specifier

Defines no structure; 2D BIT is just a 2-dimensional array. This macro defines
three functions: <NAME>-UPDATE!, <NAME>-FOLD and <NAME>-BUILD!.

SUM-TYPE is used only for type declarations: each sum is declared to be this
type. (The element-type of an array itself doesn't need to be SUM-TYPE.)"
  (let* ((name (string name))
         (fname-update (intern (format nil "~A-UPDATE!" name)))
         (fname-fold (intern (format nil "~A-FOLD" name)))
         (fname-build (intern (format nil "~A-BUILD!" name))))
    `(progn
       (declaim (inline ,fname-update))
       (defun ,fname-update (bitree index1 index2 delta)
         "Destructively increments the array: array[INDEX1][INDEX2] += DELTA."
         (declare ((integer 0 #.most-positive-fixnum) index1 index2))
         (let ((length1 (array-dimension bitree 0))
               (length2 (array-dimension bitree 1)))
           (do ((i index1 (logior i (+ i 1))))
               ((>= i length1))
             (declare (fixnum i))
             (do ((j index2 (logior j (+ j 1))))
                 ((>= j length2))
               (declare (fixnum j))
               (setf (aref bitree i j)
                     (funcall ,operator (aref bitree i j) delta))))
           bitree))
       
       (declaim (inline ,fname-fold))
       (defun ,fname-fold (bitree end1 end2)
         "Returns the sum of the rectangle region: array[0][0] + array[0][1] +
... + array[END1-1][END2-1]."
         (declare ((integer 0 #.most-positive-fixnum) end1 end2))
         (let ((res ,identity))
           ,@(when sum-type `((declare (type ,sum-type res))))
           (do ((i end1 (logand i (- i 1))))
               ((zerop i))
             (declare (fixnum i))
             (do ((j end2 (logand j (- j 1))))
                 ((zerop j))
               (declare (fixnum j))
               (setf res (funcall ,operator res (aref bitree (- i 1) (- j 1))))))
           res))

       (declaim (inline ,fname-build))
       (defun ,fname-build (array)
         "Destructively constructs 2D BIT from 2D array. (You don't need to call
this constructor if what you need is a `zero-filled' BIT, because a vector
filled with the identity element is a valid BIT as it is.)"
         (let ((length1 (array-dimension array 0))
               (length2 (array-dimension array 1)))
           (dotimes (i length1)
             (dotimes (j length2)
               (let ((dest-j (logior j (+ j 1))))
                 (when (< dest-j length2)
                   (setf (aref array i dest-j)
                         (funcall ,operator (aref array i dest-j) (aref array i j)))))))
           (dotimes (j length2)
             (dotimes (i length1)
               (let ((dest-i (logior i (+ i 1))))
                 (when (< dest-i length1)
                   (setf (aref array dest-i j)
                         (funcall ,operator (aref array dest-i j) (aref array i j)))))))
           array)))))

#+(or)
(define-2d-bitree bitree
  :operator #'+
  :identity 0
  :sum-type fixnum)
