(defpackage :cp/2d-bit
  (:use #:cl)
  (:export #:define-2d-bitree))
(in-package :cp/2d-bit)

;;;
;;; 2-dimensional binary indexed tree
;;;

;; TODO: binary search
(defmacro define-2d-bitree (name &key (operator '#'+) (identity 0) sum-type)
  "OPERATOR := binary operator (comprising a commutative monoid)
IDENTITY := object (identity element of the monoid)
SUM-TYPE := nil | type specifier

Defines no structure; 2D BIT is just a 2-dimensional array. This macro defines
three functions: <NAME>-UPDATE!, <NAME>-SUM and COERCE-TO-<NAME>!.

SUM-TYPE is used only for the type declaration: each sum is declared to be this
type. (The element-type of vector itself doesn't need to be SUM-TYPE.)"
  (let* ((name (string name))
         (fname-update (intern (format nil "~A-UPDATE!" name)))
         (fname-sum (intern (format nil "~A-SUM" name)))
         (fname-coerce (intern (format nil "COERCE-TO-~A!" name))))
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
       
       (declaim (inline ,fname-sum))
       (defun ,fname-sum (bitree end1 end2)
         "Returns the sum of the rectangle region: array[0][0] + array[0][1] +
... + array[END1-1][END2-1]."
         (declare ((integer 0 #.most-positive-fixnum) end1 end2))
         (let ((res ,identity))
           ,@(when sum-type `((declare (type ,sum-type res))))
           (do ((i (- end1 1) (- (logand i (+ i 1)) 1)))
               ((< i 0))
             (declare (fixnum i))
             (do ((j (- end2 1) (- (logand j (+ j 1)) 1)))
                 ((< j 0))
               (declare (fixnum j))
               (setf res (funcall ,operator res (aref bitree i j)))))
           res))

       (declaim (inline ,fname-coerce))
       (defun ,fname-coerce (array)
         "Destructively constructs 2D BIT from 2D array."
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

#|

(define-2d-bitree bitree
  :operator #'+
  :identity 0
  :sum-type fixnum)

;|#
