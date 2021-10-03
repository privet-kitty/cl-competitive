(defpackage :cp/binary-indexed-tree
  (:use :cl)
  (:export #:define-bitree)
  (:documentation
   "Provides 1-dimensional binary indexed tree for an arbitrary commutative
monoid."))
(in-package :cp/binary-indexed-tree)

(defmacro define-bitree (name &key (operator '#'+) (identity 0) sum-type (order '#'<))
  "OPERATOR := binary operator (comprising a commutative monoid)
IDENTITY := object (identity element of the monoid)
ORDER := nil | strict comparison operator on the monoid
SUM-TYPE := nil | type specifier

Defines no structure; BIT is just a vector. This macro defines three functions:
<NAME>-UPDATE!, point-update function, <NAME>-FOLD, query function for prefix
sum, and <NAME>-BUILD!, constructor. If ORDER is given, this macro in addition
defines <NAME>-BISECT-LEFT and <NAME>-BISECT-RIGHT, binary search functions for
prefix sum. (Note that these functions work only when the sequence of prefix
sums (VECTOR[0], VECTOR[0]+VECTOR[1], ...) is monotone.)

SUM-TYPE is used only for the type declaration: each sum
VECTOR[i]+VECTOR[i+1]...+VECTOR[i+k] is declared to be this type. When SUM-TYPE
is NIL, type declaration is omitted. Note that the element type of vector itself
doesn't need to be identical to SUM-TYPE."
  (let* ((name (string name))
         (fname-update (intern (format nil "~A-UPDATE!" name)))
         (fname-fold (intern (format nil "~A-FOLD" name)))
         (fname-build (intern (format nil "~A-BUILD!" name)))
         (fname-bisect-left (intern (format nil "~A-BISECT-LEFT" name)))
         (fname-bisect-right (intern (format nil "~A-BISECT-RIGHT" name))))
    `(progn
       (declaim (inline ,fname-update))
       (defun ,fname-update (bitree index delta)
         "Destructively increments BITREE at INDEX: BITREE[INDEX] += DELTA."
         (let ((len (length bitree)))
           (do ((i index (logior i (+ i 1))))
               ((>= i len) bitree)
             (declare ((mod #.array-dimension-limit) i))
             (setf (aref bitree i)
                   (funcall ,operator (aref bitree i) delta)))))

       (declaim (inline ,fname-fold))
       (defun ,fname-fold (bitree end)
         "Returns the prefix sum: vector[0] + ... + vector[END-1]."
         (declare ((mod #.array-dimension-limit) end))
         (let ((res ,identity))
           ,@(when sum-type `((declare (type ,sum-type res))))
           (do ((i end (logand i (- i 1))))
               ((zerop i) res)
             (declare ((mod #.array-dimension-limit) i))
             (setq res (funcall ,operator res (aref bitree (- i 1)))))))

       (declaim (inline ,fname-build))
       (defun ,fname-build (vector)
         "Destructively constructs BIT from VECTOR. (You don't need to call this
constructor if what you need is a `zero-filled' BIT, because a vector filled
with the identity element is a valid BIT as it is.)"
         (loop with len = (length vector)
               for i below len
               for dest-i = (logior i (+ i 1))
               when (< dest-i len)
               do (setf (aref vector dest-i)
                        (funcall ,operator (aref vector dest-i) (aref vector i)))
               finally (return vector)))

       ,@(when order
           `((declaim (inline ,fname-bisect-left))
             (defun ,fname-bisect-left (bitree value)
               "Returns the least index such that BITREE[0]+ ... + BITREE[index]
>= VALUE. Returns the length of BITREE if BITREE[0]+ ... +BITREE[length-1] <
VALUE. Note that this function deals with a **closed** interval."
               (declare (vector bitree))
               (if (not (funcall ,order ,identity value))
                   0 ; FIXME: should we throw an error here?
                   (let ((len (length bitree))
                         (index+1 0)
                         (sum ,identity))
                     (declare ((mod #.array-dimension-limit) index+1)
                              ,@(when sum-type
                                  `((type ,sum-type sum))))
                     (do ((delta (ash 1 (- (integer-length len) 1))
                                 (ash delta -1)))
                         ((zerop delta) index+1)
                       (declare ((mod #.array-dimension-limit) delta))
                       (let ((next-index (+ index+1 delta -1)))
                         (when (< next-index len)
                           (let ((next-sum (funcall ,operator sum (aref bitree next-index))))
                             ,@(when sum-type
                                 `((declare (type ,sum-type next-sum))))
                             (when (funcall ,order next-sum value)
                               (setq sum next-sum)
                               (incf index+1 delta)))))))))
             (declaim (inline ,fname-bisect-right))
             (defun ,fname-bisect-right (bitree value)
               "Returns the least index such that BITREE[0]+ ... + BITREE[index]
> VALUE. Returns the length of BITREE if BITREE[0]+ ... +BITREE[length-1] <=
VALUE. Note that this function deals with a **closed** interval."
               (declare (vector bitree))
               (if (funcall ,order value ,identity)
                   0
                   (let ((len (length bitree))
                         (index+1 0)
                         (sum ,identity))
                     (declare ((mod #.array-dimension-limit) index+1)
                              ,@(when sum-type
                                  `((type ,sum-type sum))))
                     (do ((delta (ash 1 (- (integer-length len) 1))
                                 (ash delta -1)))
                         ((zerop delta) index+1)
                       (declare ((mod #.array-dimension-limit) delta))
                       (let ((next-index (+ index+1 delta -1)))
                         (when (< next-index len)
                           (let ((next-sum (funcall ,operator sum (aref bitree next-index))))
                             ,@(when sum-type
                                 `((declare (type ,sum-type next-sum))))
                             (unless (funcall ,order value next-sum)
                               (setq sum next-sum)
                               (incf index+1 delta))))))))))))))

#+(or)
(define-bitree bitree
  :operator #'+
  :identity 0
  :sum-type fixnum
  :order #'<)
