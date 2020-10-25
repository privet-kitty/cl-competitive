(defpackage :cp/abstract-disjoint-set
  (:use :cl)
  (:export #:define-disjoint-set)
  (:documentation "Disjoint set by Union-Find algorithm over arbitrary
  monoid (union by size & path compression)"))
(in-package :cp/abstract-disjoint-set)

(defmacro define-disjoint-set (name &key (op '#'+) (identity 0) (element-type 'fixnum) (union-by-size t) conc-name)
  "Defines a disjoint set over arbitrary monoid.

OP is a binary operator comprising a monoid. It doesn't need to be commutative,
but a non-commutative operator doesn't make sense in many cases. (Since OP is
always called with (funcall OP <value of smaller index> <value of larger
index>), non-commutative operator will make sense when union is always applied
to adjacent components.)"
  (check-type name symbol)
  (let* ((conc-string (if conc-name
                          (symbol-name conc-name)
                          (format nil "~A-" (symbol-name name))))
         (constructor (intern (format nil "MAKE-~A" (symbol-name name))))
         (rooter (intern (format nil "~AROOT" conc-string)))
         (reffer (intern (format nil "~AREF" conc-string)))
         (uniter (intern (format nil "~AUNITE!" conc-string)))
         (connectivity-checker (intern (format nil "~ACONNECTED-P" conc-string)))
         (size-getter (intern (format nil "~ASIZE" conc-string)))
         (data-accessor (intern (format nil "~ADATA" conc-string)))
         (values-accessor (intern (format nil "~AVALUES" conc-string))))
    `(progn
       (defstruct (,name
                   (:constructor ,constructor
                       (size
                        &optional
                        (contents (make-array size
                                              :element-type ',element-type
                                              :initial-element ,identity))
                        &aux
                        (values
                         (prog1 contents
                           (assert (= (length contents) size))))
                        (data (make-array size :element-type 'fixnum :initial-element -1))))
                   ,@(when conc-name `((:conc-name ,(intern conc-string)))))
         (data nil :type (simple-array fixnum (*)))
         (values nil :type (simple-array ,element-type (*))))

       (declaim (inline ,rooter)
                (ftype (function * (values (mod #.array-total-size-limit) &optional))
                       ,rooter))
       (defun ,rooter (,name x)
         "Returns the root of X."
         (declare ((mod #.array-total-size-limit) x))
         (let ((data (,data-accessor ,name)))
           (labels ((recur (x)
                      (if (< (aref data x) 0)
                          x
                          (setf (aref data x) (recur (aref data x))))))
             (recur x))))
       
       (declaim (inline ,reffer))
       (defun ,reffer (,name x)
         (aref (,values-accessor ,name)
               (,rooter ,name x)))
       
       (declaim (inline (setf ,reffer)))
       (defun (setf ,reffer) (new-value ,name x)
         (setf (aref (,values-accessor ,name)
                     (,rooter ,name x))
               new-value))
       
       (declaim (inline ,uniter))
       (defun ,uniter (,name x1 x2)
         "Destructively unites X1 and X2 and returns true iff X1 and X2 become
connected for the first time. (If UNION-BY-SIZE is disabled, X1 becomes root.)"
         (let ((root1 (,rooter ,name x1))
               (root2 (,rooter ,name x2)))
           (unless (= root1 root2)
             (let* ((data (,data-accessor ,name))
                    (values (,values-accessor ,name)))
               ;; ensure the size of root1 >= the size of root2
               ,@(when union-by-size
                   '((when (> (aref data root1) (aref data root2))
                       (rotatef root1 root2))))
               (incf (aref data root1) (aref data root2))
               (setf (aref values root1)
                     (funcall ,op (aref values (min root1 root2)) (aref values (max root1 root2))))
               (setf (aref data root2) root1)))))

       (declaim (inline ,connectivity-checker))
       (defun ,connectivity-checker (,name x1 x2)
         "Returns true iff X1 and X2 have the same root."
         (= (,rooter ,name x1) (,rooter ,name x2)))

       (declaim (inline ,size-getter))
       (defun ,size-getter (,name x)
         "Returns the size of the connected component to which X belongs."
         (- (aref (,data-accessor ,name)
                  (,rooter ,name x)))))))

#+(or)
(define-disjoint-set disjoint-set
  :op #'max
  :identity 0
  :element-type fixnum
  :conc-name ds-
  :union-by-size nil)
