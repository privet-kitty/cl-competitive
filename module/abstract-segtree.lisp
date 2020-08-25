;;;
;;; 1-dimensional segment tree on arbitrary monoid (bottom-up implementation)
;;;

;; TODO: test
;; TODO: binary search

(defpackage :cp/abstract-segtree
  (:use :cl)
  (:export #:define-segtree))
(in-package :cp/abstract-segtree)

(defmacro define-segtree (name &key (operator '#'+) (identity 0) element-type)
  "OPERATOR := binary operator (comprising a monoid)
IDENTITY := object (identity element of the monoid)
ELEMENT-TYPE := type specifier

This macro defines three functions: <NAME>-REF, index-access function,
<NAME>-FOLD, query function for range sum, and MAKE-<NAME>, constructor."
  (let* ((fname-ref (intern (format nil "~A-REF" (symbol-name name))))
         (fname-fold (intern (format nil "~A-FOLD" (symbol-name name))))
         (fname-make (intern (format nil "MAKE-~A" (symbol-name name))))
         (fname-%make (intern (format nil "%MAKE-~A" (symbol-name name))))
         (fname-n (intern (format nil "%~A-N" (symbol-name name))))
         (fname-vector (intern (format nil "%~A-VECTOR" (symbol-name name))))
         (conc-name (intern (format nil "%~A-" (symbol-name name)))))
    `(progn
       (defstruct (,name (:constructor ,fname-%make
                             (vector &aux (n (ash (+ 1 (length vector)) -1))))
                         (:conc-name ,conc-name))
         (n nil :type (integer 0 #.(floor array-total-size-limit 2)))
         (vector nil :type (simple-array ,element-type (*))))
       (declaim (inline ,fname-make))
       (defun ,fname-make (size &key (initial-element ,identity) initial-contents)
         (declare ((integer 0 #.most-positive-fixnum) size)
                  ((or null sequence) initial-contents))
         (let ((res (make-array (- (* 2 size) 1)
                                :element-type ',element-type
                                :initial-element initial-element)))
           (when initial-contents
             (replace res initial-contents :start1 (- size 1)))
           (loop for i from (- size 2) downto 0
                 do (setf (aref res i)
                          (funcall ,operator
                                   (aref res (+ (* 2 i) 1)) (aref res (+ (* 2 i) 2)))))
           (,fname-%make res)))

       (declaim (inline ,fname-ref))
       (defun ,fname-ref (,name index)
         "Returns the element at INDEX."
         (declare ((integer 0 #.most-positive-fixnum) index))
         (aref (,fname-vector ,name)
               (+ index (,fname-n ,name) -1)))

       (declaim (inline (setf ,fname-ref)))
       (defun (setf ,fname-ref) (new-value ,name index)
         (declare ((integer 0 #.most-positive-fixnum) index)
                  (,element-type new-value))
         (let* ((vector (,fname-vector ,name))
                (i (+ index (- (,fname-n ,name) 1))))
           (declare ((integer 0 #.most-positive-fixnum) i))
           (setf (aref vector i) new-value)
           (loop while (> i 0)
                 do (setq i (ash (- i 1) -1))
                    (setf (aref vector i)
                          (funcall ,operator
                                   (aref vector (+ (* 2 i) 1))
                                   (aref vector (+ (* 2 i) 2)))))
           new-value))

       (declaim (inline ,fname-fold))
       (defun ,fname-fold (,name left right)
         "Folds the given half-open range [LEFT, RIGHT)."
         (declare ((integer 0 #.most-positive-fixnum) left right))
         (let* ((vector (,fname-vector ,name))
                (l (+ left (,fname-n ,name) -1))
                (r (+ right (,fname-n ,name) -1))
                (lvalue ,identity)
                (rvalue ,identity))
           (declare ((integer 0 #.most-positive-fixnum) l r)
                    (,element-type lvalue rvalue))
           (loop while (< l r)
                 when (evenp l)
                 do (setq lvalue (funcall ,operator lvalue (aref vector l)))
                    (incf l)
                 when (evenp r)
                 do (decf r)
                    (setq rvalue (funcall ,operator (aref vector r) rvalue))
                 do (setq l (ash (- l 1) -1)
                          r (ash (- r 1) -1)))
           (funcall ,operator lvalue rvalue))))))


#+(or)
(define-segtree segtree
  :operator #'+
  :identity 0
  :element-type fixnum)
